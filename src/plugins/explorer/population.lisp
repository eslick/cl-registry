(in-package :registry)

(registry-proclamations)

;; ==================================================================
;;  Base constraint class
;; ==================================================================

(defparameter *constraint-cache-lifetime* #.(* 60 10)) ;; 10 minutes

(defpclass constraint ()
  ((users :accessor constraint-users :initform nil :transient t)
   (time  :accessor constraint-time-computed :initarg :time :initform 0)))

(defmethod constraint-user-oids :around ((constraint constraint) &key force &allow-other-keys)
  "Cache returned OIDs"
  (declare (ignore force))
  (with-slots (users time) constraint
;;    (if (and (not force)
;	     (< (time-since time) *constraint-cache-lifetime*)
;	     users)
;	users
	(progn 
	  (setf time (get-universal-time))
	  (setf users (call-next-method)))))

(defun time-since (time)
  (- (get-universal-time) time))

;; ==================================================================
;;  Represents a subset of the patient population
;; ==================================================================

(defparameter *population-minimum-size* 1)

(defmodel population (constraint fulltext-mixin)
  ((name :accessor population-name :initarg :name :initform "")
   (keywords :accessor population-keywords :initarg :keywords :initform "")
   (constraints :accessor population-constraints :initarg :constraints 
		:initform nil)))

(defmethod fulltext-fields ((instance population))
  '(name keywords))

(defmethod humanize-name ((pop population))
  (safe-subseq (population-name pop) 0 40))

(defmethod attributize-name ((pop population))
  (attributize-name (population-name pop)))

(defun make-population (constraints &key (name "") (keywords ""))
  (let ((pop (make-instance 'population 
			    :constraints constraints
			    :name name
			    :keywords keywords)))
    (setf (constraint-users pop)
	  (compute-users (population-constraints pop)))
    (setf (constraint-time-computed pop)
	  (get-universal-time))))

(defun compute-users (constraints &key force)
  (mapcar (curry 'elephant::controller-recreate-instance *store-controller*)
	  (compute-user-oids constraints force)))

(defun compute-user-oids (constraints &optional force)
  (remove-if (lambda (x) (member x (blacklisted-user-oids)))
	     (apply 'intersections 
		    (all-patient-oids)
		    (mapcar (lambda (c) 
			      (sort (constraint-user-oids c :force force) #'>))
			    (remove-nulls 
			     (remove-duplicates 
			      (flatten-constraints constraints)))))))

(defun flatten-constraints (constraints)
  (cond ((null constraints)
	 nil)
	((subtypep (type-of (first constraints)) 'population)
	 (append (population-constraints (first constraints))
		 (flatten-constraints (rest constraints))))
        (t (cons (first constraints) (flatten-constraints (rest constraints))))))

(defmethod constraint-user-oids ((population population) &key force &allow-other-keys)
  "Constructive constraints; one population can constraint another"
  (apply 'intersections (mapcar (lambda (c) (constraint-user-oids c :force force))
				(population-constraints population))))

;; ==================================================================
;; Computing constraints from population presentations
;; ==================================================================

(defun presentations-to-constraints (presentations)
  "Compute a set of constraint from a set of question presentations"
  (mapcar #'presentation-to-constraint presentations))

(defmacro presentation-to-constraint-creator (presentation-type value-type value-expr)
  `(defmethod presentation-to-constraint ((presentation ,presentation-type))
     (assert (metadata presentation))
     (make-instance 'question-constraint 
		    :question (metadata presentation)
		    :type ,value-type
		    :value ,value-expr)))

(presentation-to-constraint-creator boolean-presentation :value 
				    (lisp-value presentation))

(presentation-to-constraint-creator number-presentation :value
				    (lisp-value presentation))

(presentation-to-constraint-creator range-integer-presentation :range
				    (lisp-value presentation))

(presentation-to-constraint-creator member-choice-presentation :choices
				    (mklist (lisp-value presentation)))


;; ==================================================================
;;  Filter a population by their answers to questions
;; ==================================================================

;; Type may be one of:
;; - value (text; "foo", 27)
;; - range (text field; "20-40", "> 20, < 40" -> '(20 40))
;; - open-range (text field: "> 20" -> '(> 20))
;; - choices (multichoice; (list choices))

(defpclass question-constraint (constraint)
  ((question :accessor constraint-question :initarg :question 
	     :initform nil)
   (type :accessor constraint-type :initarg :type)
   (value :accessor constraint-value :initarg :value)))

(defmethod constraint-user-oids ((constraint question-constraint) &key force &allow-other-keys)
  (declare (ignore force))
  (loop for answer in (get-answers (constraint-question constraint)) 
     when (match-value-constraint constraint (value answer))
       collect (object-id (user answer))))
     

(defun match-value-constraint (constraint reference)
  (with-slots (type value) constraint
    (case type
      (:value (equalp reference value))
;;      (:string (some value :test #'equalp) (tokenize string)
      (:range (and (> (car value) reference)
		   (< (cdr value) reference)))
      (:open-range (funcall (car value) reference (cdr value)))
      (:choices (member reference value :test #'equal)))))

(defmethod humanize-constraint ((constraint question-constraint))
  (question-prompt (constraint-question constraint)))

;; ==================================================================
;;  Constrain a population by preference
;; ==================================================================

(defpclass preference-constraint (constraint)
  ((preference :accessor constraint-preference :initarg :preference)
   (type :accessor constraint-type :initarg :type)
   (value :accessor constraint-value :initarg :value)))
  
(defmethod constraint-user-oids ((constraint preference-constraint) &key force &allow-other-keys)
  (declare (ignore force))
  (loop for user in (all-users) 
       for value = (get-preference (constraint-preference constraint) user)
       when (match-value-constraint constraint value)
       collect (object-id user)))

(defmethod humanize-constraint ((constraint preference-constraint))
  (prompt (constraint-preference constraint)))

;; ==================================================================
;;  Comparisons between populations
;; ==================================================================

(defpclass population-comparison ()
  ((question :accessor question :initarg :question :index t)
   (main :accessor primary-population :initarg :primary)
   (comp :accessor comparison-population :initarg :comparison)))

(defmethod humanize-name ((comp population-comparison))
  (concatenate 'string
	       "Compare: "
	       (safe-subseq (question-name (question comp)) 0 25)))
      

(defun make-population-comparison (question mpop cpop)
  (aif-ret (lookup-comparison question mpop cpop)
    (make-instance 'population-comparison 
		   :question question
		   :primary mpop
		   :comparison cpop)))

(defun lookup-comparison (question mpop cpop)
  (awhen (select-if (lambda (inst)
		      (eq (primary-population inst) mpop))
		    (question-comparisons question))
    (find cpop it :key 'comparison-population)))

(defun question-comparisons (question)
  (get-instances-by-value 'population-comparison 'question question))
