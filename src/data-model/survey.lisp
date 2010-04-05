(in-package :registry)

(registry-proclamations)

;; ===========================================================
;;  Survey data model
;; ===========================================================

(defmodel survey (fulltext-mixin user-translation-mixin)
  (;; Basic description
   (name :accessor name :initarg :name :initform "" :index t)
   (description :accessor description :initarg :description :initform "")
   (help :accessor help :initarg :help :initform "")
   ;; Groups
   (groups :accessor survey-groups :initarg :groups :initform '()
	   :documentation "An ordered list of groups; replaces default rules")
   (archived-questions :accessor archived-questions :initarg :archives :initform nil)
   ;; Diaries
   (diary-p :accessor diary-p :initarg :diary-p :initform nil
	    :documentation "Is the whole survey a diary entry (single group)?")
   (diary-question :accessor diary-question 
		   :initarg :diary-question
		   :initform nil)
   (diary-description :accessor diary-description 
		      :initarg :diary-description
		      :initform nil)
   ;; Access control
   (published :accessor published-p :initarg :published :initform nil)
   (priority :accessor priority-value :initarg :priority :initform nil)
   (owner :accessor owner :initarg :owner :initform nil)
   (sticky :accessor sticky-p :initarg :sticky :initform nil)
   (origin :accessor origin :initarg :origin :initform nil)
   (sort-key :reader sort-key)   ;value computed by slot-value-using-class method below
   (acl :accessor survey-acl :initarg :acl :initform nil)
   (edit-lock :initarg :edit-lock :initform nil
	      :transient t
	      :documentation "Session of user editing the survey.")
   (formats :accessor formats :initarg :formats :initform nil
            :documentation "Plist of key/value pairs for survey formatting properties")
   ;; Ranking
   (rating :accessor survey-rating :initarg :rating :initform nil)
   (ranking-record :accessor ranking-record :initarg :ranking-record
		   :initform (make-ranking-record :ranking nil :distribution #(0 0 0 0 0))))
  (:documentation "The survey object represents a coherent branching survey.
     It has a set of root questions or groups which may contain branching
     information.  It also provides some basic ACL support for editing."))

(defmethod elephant::slot-value-using-class ((class elephant:persistent-metaclass)
                                             (instance survey)
                                             slot-def)
  (if (neq (elephant::slot-definition-name slot-def) 'sort-key)
      (call-next-method)
      (survey-sort-key instance)))

;; Inspectors and properties

(defmethod print-object ((inst survey) stream)
  (format stream "#<SURVEY-~A '~A'>"
	  (object-id inst)
	  (name inst)))

(defmethod fulltext-fields ((instance survey))
  '(name description help))

(defmethod translate-fields ((obj survey))
  '(name description help))

(defmethod humanize-name ((inst survey))
  (humanize-name (name inst)))

(defmethod attributize-name ((inst survey))
  (attributize-name (name inst)))

(defmethod get-format ((inst survey) key)
  (setq key (as-keyword key))
  (getf (formats inst) key))

(defmethod set-format ((inst survey) key val)
  (setq key (as-keyword key))
  (setf (getf (formats inst) key) val))

(defmethod (setf get-format) (val (inst survey) key)
  (set-format inst key val))

;; Manipulating surveys and child groups

(defun find-group-by-name (survey name)
  (find name (survey-groups survey) :key #'group-name :test #'equal))

(defmethod get-survey (id)
  (assert-body-type 'survey
    (typecase id
      (number (get-model 'survey id))
      (string (get-instance-by-value 'survey 'name id)))))

(defmethod drop-instance ((obj survey))
  (drop-survey obj)
  (call-next-method))

(defun drop-survey (survey)
  "Drop survey and all dependent data"
  (declare (ignore survey))
  nil)
;;  (when (yes-or-no-p "Are you really sure you want to delete ~A and all descendant groups and questions?" survey))
;;    (dolist (group (survey-groups survey))
;;      (drop-group group))))

(defun survey-editors (survey)
  (remove-duplicates 
   (remove-nulls 
    (cons (owner survey)
	  (survey-acl survey)))))


;; Actions

(defmethod publish-object ((obj survey))
  (setf (published-p obj) t))

(defmethod unpublish-object ((obj survey))
  (setf (published-p obj) nil))

(defun published-surveys ()
  (select-if #'published-p (get-instances-by-class 'survey)))

(defun survey-complete-p (user survey)
  (every (curry 'group-answered-p user) (survey-groups survey)))


;; This is not thread-safe, but locking surveys is fragile anyway,
;; and there are easier ways to lose than this...
(defmethod edit-lock ((obj survey))
  (when (slot-value obj 'edit-lock)
    (loop for (nil . session) in hunchentoot::*session-db*
	 do (when (and (not (hunchentoot:session-too-old-p session))
		       (eq session (slot-value obj 'edit-lock)))
	      (return-from edit-lock session))))
;;    (log-message :survey :debug "Found stale edit-lock, removing..."))
  (setf (slot-value obj 'edit-lock) nil))

(defmethod (setf edit-lock) (new-value (obj survey))
  (setf (slot-value obj 'edit-lock) new-value))

;;
;; Admin and other standard views
;;

(defview survey-simple-view (:type table)
  name
  (owner :reader 'owner-name)
  (published :present-as predicate))

(defun owner-name (survey)
  (awhen (owner survey)
    (username it)))

(defview survey-table-view (:type table)
  name
  (description :reader 'translated-slot-reader)
  (published :present-as predicate))

;; Maybe this belongs in Weblocks
(defmethod view-field-label :around (view)
  (let ((res (call-next-method view)))
    (if (functionp res) (funcall res) res)))

(defun sort-char-to-color (sort-char)
  (or (cdr (assoc sort-char
                  '((#\0 . "red")
                    (#\1 . "orange")
                    (#\2 . "green")
                    (#\3 . "blue")
                    (#\4 . "black"))))
      "black"))

(defclass sort-key-presentation (presentation)
  ())

(defmethod render-view-field-value (value (presentation sort-key-presentation)
                                    field view widget obj &rest args)
  (declare (ignore field view widget obj args))
  (let ((char (and (stringp value) (> (length value) 0) (elt value 0))))
    (with-html
      (:div :style (format nil "background-color: ~a;" (sort-char-to-color char))
             "&nbsp;"))))

(defview survey-viewer-view (:type table)
  (name :reader (translated-slot-reader 'name) :label (lambda () #!"Name"))
  (description :reader (translated-slot-reader 'description) :label (lambda () #!"Description"))
  (sort-key :label (lambda () #!"Origin") :present-as sort-key))

(defview survey-form-view (:type form :inherit-from '(:scaffold survey))
  (owner :present-as (dropdown :choices #'(lambda (x) 
					    (declare (ignore x))
					    (all-users))
 			       :label-key #'username)
	  :parse-as (mid)
	  :reader (compose #'mid #'owner))
  (description :present-as (textarea :cols 60 :rows 20))
  (published :present-as checkbox)
  (root :hidep t)
  (groups :hidep t)
  (acl :hidep t)
  (ranking-record :hidep t)
  (fulltext :hidep t))

(defview survey-data-view (:type data)
  name description (published :present-as predicate))

;;  (published :hidep t)
;;  (owner :hidep t)
;;  (root :hidep t)
;;  (groups :hidep t)
;;  (acl :hidep t)
;;  (ranking-record :hidep t)
;;  (fulltext :hidep t))
  
;;
;; Print out a survey for written review or IRB submission
;;

(defun dump-survey-to-file (survey filename)
  (with-open-file (stream filename :direction :output :if-exists :supersede)
    (dump-survey survey stream)))

(defun dump-survey (survey stream)
  (loop for group in (survey-groups survey) 
     do (dump-group group stream 0)))

(defun dump-group (group stream indent)
  (unless group (return-from dump-group))
  (let ((istr (make-string indent :initial-element #\Space)))
    (when (= indent 0)
      (format stream "~APAGE TITLE: ~A~%" istr (group-name group))
      (format stream "~A----------------------------------------~%" istr))
    (awhen (group-advice group)
      (when (> (length it) 10)
	(format stream "~A~A~%~%" istr it)))
    (loop for question in (group-questions group)
       do (dump-question question stream indent)))
  (format stream "~%~%"))

(defun dump-question (question stream indent)
  (let ((istr (make-string indent :initial-element #\Space)))
;;    (format stream "~A~A:  ____________________________ (~A)~%" 
;;	    istr (question-prompt question)
;;	    (question-data-type question))
    (format stream "~A~A: (~A)~%" 
	    istr (question-prompt question)
	    (question-data-type question))
    (when (member (question-data-type question)
		  '(:choice :multichoice))
      (format stream "~A| Choices: ~{~A~#[~:;, ~]~}~%" istr 
	      (mapcar #'car (question-choices question))))
    (awhen (question-help question)
      (when (> (length it) 5)
	(format stream "~A| \"~A\"~%" istr it)))
    (awhen (question-rules question)
      (loop for rule in it
	 do (format stream "~A| If your answer is '~A', answer the following~%~%"
		    istr (group-rule-value rule))
	   (dump-group (group-rule-target rule) stream (+ 5 indent))))
    (princ #\Newline stream)))

(defun question-rules (question)	 
  (awhen (parent question)
    (loop for rule in (group-rules it)
	 when (eq (group-rule-question rule) question)
	 collect rule)))
       


;; ==============================================================
;;  Ranking information
;; ==============================================================

(defstruct ranking-record ranking distribution)

;; ==============================================================
;; Sorting
;; ==============================================================

(defun survey-origin-choices ()
  `((,#!"Inherit" . "inherit")
    (,#!"Authenticated Professional" . "professional")
    (,#!"Researcher/Clinician" . "researcher")
    (,#!"Patient" . "patient")
    (,#!"Other" . "other")))

(defun survey-origin-to-display (origin)
  (or (car (rassoc origin (survey-origin-choices)
                  :test #'string-equal))
      #!"Inherit"))

(defun format-survey-origin (stream value &rest ignore)
  (declare (ignore ignore))
  (princ (survey-origin-to-display value) stream))

(defun survey-sort-index (survey)
  (if (sticky-p survey)
      0
      (let ((origin (or (origin survey) "inherit")))
        (cond ((string-equal origin "inherit")
               (let* ((owner (owner survey)))
                 (cond ((has-permission-p owner 'authenticated-professional t)
                        1)
                       ((researcher-p owner) 2)
                       ((or (has-preference-value-p owner :lam-patient-p t)
                            (has-preference-value-p owner :tsc-patient-p t))
                        3)
                       (t 4))))
              ((string-equal origin "professional") 1)
              ((string-equal origin "researcher") 2)
              ((string-equal origin "patient") 3)
              (t 4)))))

(defun survey-sort-key (survey)
  (format nil "~d-~a"
          (survey-sort-index survey)
          (slot-value-translation survey 'name)))
