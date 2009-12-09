(in-package :registry)

(registry-proclamations)

;; ==============================================
;;  Progress bar
;; ==============================================

(defun make-progress-bar (&optional (percent 0.50))
  (make-instance 'progress-bar :percent percent))

(defwidget progress-bar ()
  ((percent-done :accessor percent-done :initarg :percent :initform 0.0)))

(defmethod render-widget-body ((widget progress-bar) &rest args)
  (declare (ignore args))
  (with-html 
    (:script :type "text/javascript"
	     (str (js:js-to-string 
		   `(progn 
		      (defvar my-bar
			(new (progress-bar 4 "#000000" "#a5f3b1" "#043db2" 200 20 1)))
		      (.set-bar my-bar ,(percent-done widget))))))))
