(in-package :registry)

(registry-proclamations)

;; ===========================================================
;;  Study data model
;; ===========================================================


(defmodel study (fulltext-mixin user-translation-mixin)
  (;; Basic description
   (name :accessor name :initarg :name :initform "" :index t)
   (description :accessor description :initarg :description :initform "")
   (help :accessor help :initarg :help :initform "")
   ;; Surveys
   (surveys :accessor surveys :initarg :surveys :initform '()
	   :documentation "An ordered list of surveys")
   (survey-rules :accessor survey-rules :initarg :rules :initform '()
		 :documentation "A list of survey rules")
   ;; Patient consent process
   (requires-consent-p :accessor requires-consent-p
		       :initarg :requires-consent-p
		       :initform nil)
   (patient-consent-forms :accessor patient-consent-forms
			  :initarg :patient-consent-forms
			  :initform nil)
   (patients-consented :accessor patients-consented :initform '()
		       :documentation "A list of patient MIDs")
   ;; Access control
   (published :accessor published-p :initarg :published :initform nil)
   (priority :accessor priority-value :initarg :priority :initform nil)
   (owner :accessor owner :initarg :owner :initform nil)
   (sticky :accessor sticky-p :initarg :sticky :initform nil)
   (origin :accessor origin :initarg :origin :initform nil))
  (:documentation "The study object represents a sequence of surveys."))

;; Inspectors and properties

(defmethod print-object ((inst study) stream)
  (format stream "#<STUDY-~A '~A'>"
	  (object-id inst)
	  (name inst)))

(defmethod fulltext-fields ((instance study))
  '(name description help))

(defmethod translate-fields ((obj study))
  '(name description help))

(defmethod humanize-name ((inst study))
  (humanize-name (name inst)))

(defmethod attributize-name ((inst study))
  (attributize-name (name inst)))

;; Manipulating studies and child surveys

(defun find-survey-by-name (study name)
  (find name (surveys study) :key #'name :test #'equal))

(defmethod get-study (id)
  (assert-body-type 'study
    (typecase id
      (number (get-model 'study id))
      (string (get-instance-by-value 'study 'name id)))))

(defmethod drop-instance ((obj study))
  (drop-study obj)
  (call-next-method))

(defun drop-study (study)
  "Drop study and all dependent data"
  (when (yes-or-no-p "Are you really sure you want to delete ~A and all descendant surveys?" study)
    (dolist (survey (surveys study))
      (drop-survey survey))))

;; ========================================================================
;; Actions
;; ========================================================================

;; Publishing

(defmethod publish-object ((obj study))
  (setf (published-p obj) t))

(defmethod unpublish-object ((obj study))
  (setf (published-p obj) nil))

(defun published-studies ()
  (select-if #'published-p (get-instances-by-class 'study)))

;; Study completion

(defun study-complete-p (patient study)
  (every (curry 'survey-complete-p patient) (surveys study)))

;; Patient consent process

(defmethod study-patient-consented-p ((study study) (patient patient))
  (find (mid patient) (patients-consented study)))

(defmethod set-study-patient-consented-p ((study study) (patient patient) value)
  (setf (patients-consented study)
	;; TODO: patient consent history!!
	(if value
	    (pushnew (mid patient) (patients-consented study))
	    (remove (mid patient) (patients-consented study)))))

(defmethod (setf study-patient-consented-p) (value study patient)
  (set-study-patient-consented-p study patient value))
