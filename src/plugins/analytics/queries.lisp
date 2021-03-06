(in-package :registry)

(registry-proclamations)

;; =================================================
;;  Dataset utilities
;; =================================================

;;
;; Discrete distributions
;;

(defun distribution-query (question viz)
  (make-categorical-distribution 
   question (question-data-type question) (constraints viz)))

(defmethod make-categorical-distribution ((q question) (type (eql :number)) 
					  constraints &rest args)
  (apply 'make-histogram-distribution q type constraints args))

(defmethod make-categorical-distribution ((q question) (type (eql :measurement)) 
					  constraints &rest args)
  (apply 'make-histogram-distribution q type constraints args))

(defmethod make-categorical-distribution ((q question) (type (eql :choice))
					  constraints &rest args)
  "Returns pairs of values in the series and the number of times they occur"
  (declare (ignore args))
  (convert-to-google-dataset
   (list (get-column-header q)
	 '("number" "Number of Respondents"))
   (cons->list (compute-distribution (flatten (get-answer-values* q constraints))
				     :test #'equal))))

(defmethod make-categorical-distribution ((q question) (type (eql :multichoice))
					  constraints &rest args)
  (apply #'make-categorical-distribution q :choice constraints args))

(defmethod make-categorical-distribution ((q question) (type (eql :boolean))
					  constraints &rest args)
  (declare (ignore args))
  (convert-to-google-dataset
   (list (get-column-header q)
	 (copy-list '("number" "Number of Respondents")))
   (cons->list 
    (sort (compute-distribution (flatten 
				 (mapcar #'(lambda (x)
					     (if x "yes" "no"))
					 (get-answer-values* q constraints)))
				:test #'equal
				:sort nil)
	  (lambda (a b)
	    (if (and (equal a "no")
		     (equal b "yes"))
		nil
		t))
	  :key #'car))))
	    

;; (defmethod make-categorical-distribution ((q question) (type (eql :number)) 
;; 				     constraints &key (sort #'sort-any->=) &allow-other-keys)
;;   (convert-to-google-dataset
;;    (list (get-column-header q)
;; 	 (copy-list '("number" "Number of Respondents")))
;;    (cons->list 
;;     (compute-distribution (get-answer-values* q constraints) :sort sort))))


(defun compute-distribution (series &key (test #'eq) (sort #'sort-any->=));; key (format :append))
  "Format is :append (add count to back of list), prepend (to front) 
   (value :pair, :none "
  (let ((table (make-hash-table :test test)))
    (loop for elt in series do
;;	 for value = (if key (funcall key elt) elt) do
	 (handler-case 
	     (incf (gethash elt table))
	   (simple-type-error ()
	       (setf (gethash elt table) 1))
	   (type-error ()
	       (setf (gethash elt table) 1))))
    (let ((items (if sort
		     (sort (hash-items table) sort :key #'cdr)
		     (hash-items table))))
      items)))


;;
;; Continuous distributions
;;

;; Plot over values on the x-axis via histogram/binning of counts

(defparameter *default-bins* 10)

(defun histogram-query (question viz &optional constraints)
  (make-histogram-distribution question (question-data-type question) 
			       (or constraints (constraints viz))))

(defmethod make-histogram-distribution ((q question) (type (eql :date)) constraints)
  (make-histogram-distribution q :number constraints))

(defmethod make-histogram-distribution ((q question) (type (eql :number)) constraints)
  (convert-to-google-dataset 
   (list (get-column-header q)
	 '("number" "Response Count"))
   (let ((values (get-answer-values* q constraints)))
     (when values (compute-histogram values)))))

(defmethod make-histogram-distribution ((q question) (type (eql :measurement)) constraints)
  (let ((lunit (get-locale-unit 
		(canonical-unit-for-measurement 
		 (question-data-subtype q)))))
    (convert-to-google-dataset 
     (list (get-column-header q)
	   '("number" "Response Count"))
     (compute-histogram 
      (mapcar #'(lambda (value)
		  (convert-from-canonical value lunit))
	      (get-answer-values* q constraints))))))

(defun compute-histogram (series &key (bins *default-bins*) &allow-other-keys)
  (declare (optimize (speed 1) (space 1) (debug 3) (safety 2)))
  (let* ((min (reduce #'min series))
	 (max (reduce #'max series))
	 (range (- max min))
	 (bin-size (ceiling (/ range bins)))
	 (table (make-hash-table))
	 (result nil))
    (when (= 0 bin-size) (return-from compute-histogram nil))
    (flet ((bin-number (x)
	     (floor (/ (- x min) bin-size)))
	   (bin-value (bin)
	     (+ min (* bin bin-size))))
      (loop for bin from 0 upto bins do
	 (setf (gethash bin table) 0))
      (mapc #'(lambda (value)
		(incf (gethash (bin-number value) table)))
	    series)
      (maphash (lambda (bin-number count)
		 (push (list (floor (bin-value bin-number)) count) result))
	       table)
      (mapcar (lambda (pair)
		(setf (first pair) (format nil "~D" (first pair)))
		pair)
	      (sort result #'< :key #'first)))))



;;
;; Time series
;;

;;(defmethod linear-regression (series-list)
;  "Return a line over the range that is the regression line for
;   the set of series: (x y)"

(defmethod series-query (question viz &optional constraints)
  (declare (ignore viz constraints))
  (let ((result (second (return-patient-series question (current-patient)))))
    (convert-to-google-dataset 
     (list '("string" "Date")
           `("number" ,(second (get-column-header question))))
     (or result '(("No Data" 1))))))
  
(defmethod return-patient-series (question patient)
  "Get the patient's ordered set of answers to a diary question"
  (assert (diary-question-p question))
  (let* ((reference (diary-question 
		    (first (find-group-surveys 
			    (parent question)))))
	 (references (sort (get-user-answers reference patient) #'< :key #'answer-id))
	 (values (sort (get-user-answers question patient) #'< :key #'answer-id)))
    (list (cons (question-data-type reference) 
		(question-data-type question))
	  (mapcar #'(lambda (r v)
		      (assert (eq (answer-id r) (answer-id v)))
		      (list (format-datetime-display (value r) :show-time-p nil)
			    (value v)))
		  references values))))



;; ====================================================
;;  Query support (quick hack to filter answers)
;; ====================================================

;; Blacklist

(defparameter *patient-blacklist* nil)
(defparameter *patient-blacklist-oids* nil)

(defun blacklisted-patient-p (patient)
  (unless *patient-blacklist*
    (setf *patient-blacklist*
	  (mapcar #'get-patient-home-patient
                  (delete nil
                          (mapcar #'get-user
                                  '("eslick" "rme" "charlest" "Charles" "clozure" 
                                    "Dugan" "david" "lpolzer")))))
    (setf *patient-blacklist-oids* 
	  (mapcar #'object-id *patient-blacklist*)))
  (when (member patient *patient-blacklist*) t))

(defun blacklisted-patient-oids ()
  (blacklisted-patient-p nil)
  *patient-blacklist-oids*)

;; To do: move ALL-PATIENTS and friends into some place like core/models/model-utils.lisp

(defun all-patients (&key center test)
  "Returns all PATIENT objects, optionally filtered by CENTER and TEST"
  ;; The semantics of "all patients" depends somewhat on the portal / application.
  ;;
  ;; In the Medical Registry, all user input to surveys is taken from a patient object.
  ;; But not all users are patients.
  ;;
  ;; In LAMsight, a patient is a user if they set a preference. (attached to user object)
  ;; In ILR, a patient is a patient, but ALL-PATIENTS is generally limited to the current center.
  ;; In the object model,  patients and users are related, but they are not synonymous.
  ;; A PATIENT object may contain a USER object.  (LAMsight legacy)
  ;; A logged-in USER may operate on multiple PATIENT objects. (ILR modern behavior)
  ;; Hence the concept of an "identification mode" for ALL-PATIENTS-BY-IDENTIFICATION-MODE.
  (check-type center (or null center))
  (check-type test (or null symbol function))
  (select-if (or test #'identity)
	     (if center
		 (get-patients-for-center center)
		 (get-instances-by-class 'patient))))

(defvar *all-patients-identification-mode* ':config
  "One of (:ALL :CONFIG :ILR :LAMSIGHT)
 :ALL Accept all patient objects
 :CONFIG Check site config parameter ALL-PATIENTS-IDENTIFICATION-MODE
 :ILR All patient objects for current center
 :LAMSIGHT Check user preferences
")

(defun check-all-patients-identification-mode ()
  (if (or (null *all-patients-identification-mode*)
	  (eq  *all-patients-identification-mode* ':config))
      (setq *all-patients-identification-mode*
	    (get-site-config-param ':all-patients-identification-mode)))
  (check-type *all-patients-identification-mode* (member :all :ilr :lamsight))
  ;; Returns
  *all-patients-identification-mode*)

(defun all-patients-by-identification-mode ()
  "Returns all patient objects determined by mode: :LAMSIGHT, :ILR, or :ALL"
  (check-all-patients-identification-mode)
  (ecase *all-patients-identification-mode*
    (:lamsight
     (all-patients :center (get-patient-home-center) :test #'patient-p))
    (:ilr
     (all-patients :center (current-center) :test #'patient-p))
    (:all
     (all-patients :center nil :test #'identity))))

(defparameter *total-patients* nil)

(defun total-patients ()
  (unless *total-patients*
    (setf *total-patients*
	  (length (all-patients-by-identification-mode))))
  *total-patients*)

;;   (unless *patient-question* 
;;     (setf *patient-question* (get-question 5818)))
;;   (let ((answer (get-answer *patient-question* user)))
;;     (when answer
;;       (member "Patient" (value answer) :test #'equalp))))

(defmethod get-answer-values* ((q question) constraints)
  "Does some default cleaning of the data"
  (get-answer-values q constraints))

(defmethod get-answer-values ((q question) constraints)
  (ensure-transaction ()
    (mvbind (answers popcount)
        (get-filtered-answers q constraints)
      (values (mapcar #'safe-value answers) popcount))))

(defun get-filtered-answers (question constraints)
  "NOTE: We can clean this up by implementing an oid join on 
   indexed slots containing objects"
  (let ((users (compute-patients constraints)))
    (values (select-if (lambda (x) 
			 (and (member (user x) users)
			      (not (eq (value x) :unknown))))
		       (get-answers question))
	    (length users))))

(defun safe-value (answer)
  (when (slot-boundp answer 'value)
    (value answer)))


;; =================================================
;;  Simple Question Datasets
;; =================================================

(defun get-column (question answers &key cid)
  "Take a list of answers and generate a table column"
  (cons (get-column-header question cid)
	(get-column-data answers)))

(defun get-column-data (answers)
  (mapcar #'safe-value answers))

(defun get-column-header (question &optional cid)
  `(,(get-column-data-type question)
    ,(get-column-label question)
    ,@(when cid cid)))

(defun get-column-label (question)
  (question-prompt question))
	
(defun get-column-data-type (question)
  (ecase (question-data-type question)
    (:boolean "string")
    (:number "string")
    (:measurement "string")
    (:string "string")
    (:date "date")
;;    (:range "number")
    (:choice "string")
    (:multichoice "string")))


;;
;; General utilities
;;

(defun sort-any->= (a b)
  (elephant::lisp-compare>= a b))

(defun sort-any< (a b)
  (elephant::lisp-compare< a b))


;; =================================================
;;  Query Datasets
;; =================================================

;;
;; Convert from table to client form
;;

(defun convert-to-google-dataset (column-headers rows)
  (distribute (cons column-headers rows)))

