(in-package :registry)

(registry-proclamations)

(defparameter *default-log-level* :error)

(defvar *log-level* *default-log-level*)

(defgeneric log-error-p-function (condition)
  (:method ((condition t)) t))

#+ccl
(defmethod log-error-p-function ((condition ccl:input-timeout))
  nil)

(defun start-logging (&optional (level *default-log-level*) (acceptor weblocks::*weblocks-server*))
  "Start logging Hunchentoot web access. LEVEL should be :error, :warning, :debug, or :info"
  (set-log-level level)
  ;; Returns
  (values
   (setf (hunchentoot::acceptor-message-log-destination acceptor)
	 (make-pathname :defaults (registry-relative-path (list "logs"))
			:name "registry"
			:type "log"))
   (setf (hunchentoot::acceptor-access-log-destination acceptor)
	 (if (get-site-config-param :enable-access-logging)
	     (make-pathname :defaults (registry-relative-path (list "logs"))
			    :name "access"
			    :type "log")))))

(defun set-log-level (level)
  (setf *log-level* level)
  (cond ((member level '(:warning :info :debug))
         (setf hunchentoot:*log-lisp-errors-p* t
               hunchentoot:*log-lisp-warnings-p* t))
        ((member level '(:error :warning :info :debug))
         (setf hunchentoot:*log-lisp-errors-p* t
               hunchentoot:*log-lisp-warnings-p* nil))
        (t
         (setf hunchentoot:*log-lisp-errors-p* nil
               hunchentoot:*log-lisp-warnings-p* nil))))

(defun stop-logging (&optional (accessor weblocks::*weblocks-server*))
  (set-log-level nil)
  (setf (acceptor-message-log-destination accessor) nil))

(defun log-message (category level format &rest args)
  (when (member *log-level* (member level '(:error :warning :info :debug)))
    (apply #'hunchentoot::log-message* level 
           (format nil "(~A) ~A" category format)
           args)))
