(in-package :registry)

(registry-proclamations)

;; ==============================================================
;;  Maintaining user state in a survey
;; ==============================================================

(defpclass survey-state ()
  ((user :accessor user :initarg :user :index t)
   (survey :accessor survey :initarg :survey)
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

(defun initialize-control-from-state (ctrl state)
  (awhen (last-group state)
    (goto-group ctrl it)
    it))
