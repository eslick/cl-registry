(in-package :registry)

(registry-proclamations)

;; ==============================================================
;;  Maintaining user state in a survey
;; ==============================================================

(defpclass survey-state ()
  ((user :accessor user :initarg :user :index t)
   (survey :accessor survey :initarg :survey :index t)
   (ranking :accessor ranking :initarg :ranking :initform nil)
   (last-values :accessor last-values :initarg :last-values :initform nil)
   (last-group :accessor last-group :initarg :last-group :initform nil)
   (percent-answered :accessor percent-answered :initarg :percent-answered :initform 0)
   (finished-p :accessor finished-p :initarg :finished-p :initform nil))
  (:documentation "The state of a user survey session so we can continue later"))

(defun get-survey-state (survey user)
  (find survey (get-instances-by-value 'survey-state 'user user) :key #'survey))

(defun make-survey-state (survey user &rest args)
  (apply #'make-instance 'survey-state :survey survey :user user args))

;; TODO: DEFGENERIC SURVEY-COMPLETE-P and unify method signatures 
(defmethod survey-finished-p ((st survey-state) (patient patient) &key (all-questions nil))
  (survey-finished-p (survey st) patient :all-questions all-questions))

(defmethod survey-finished-p ((survey survey) (patient patient) &key (all-questions nil))
  (with-transaction ()
    (dolist (group
	      (loop for group in (survey-groups survey) append (find-subgroups group)))
      (dolist (question (group-questions group))
	(cond
	  ;; Checking only required questions?
	  ((and (not all-questions) (not (question-required-p question))))
	  ;; Any answers to question for user/patient?
	  ((get-user-answers question patient))
	  (t (return-from survey-finished-p nil))))))
  ;; Returns
  t)
