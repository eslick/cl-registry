(in-package :registry)

(registry-proclamations)

(defwidget simple-error ()
  ((message :accessor error-message :initarg :message :initform "Error [no message specified]"))
  (:documentation "Simple widget for showing server side errors.  Replaces default hunchentoot
   handler"))

(defmethod render-widget-body ((error simple-error) &rest args)
  (with-html
    (:h1 :style "color: #cc3333;" "ERROR")
    (<:p :style "color: #cc3333;" (str (message error)))))

;; Richer error handler

(defwidget backtrace-error ()
  ((condition :accessor error-condition :initarg :condition :initform nil)
   (backtrace :accessor error-backtrace :initarg :backtrace))
  (:documentation "Generic component for showing server side error
   conditions.  Attempts to display a backtrace."))

