(in-package :registry)

(registry-proclamations)

;;
;; System Event Hooks
;;

;; This class standardizes a set of system events 
;; I used a class so there was one place where all the supported
;; events were defined.  If you try to add to or call an unspecified 
;; hook, an error is asserted.

(defclass registry-hooks ()
  ((start-app :initarg :start :accessor start-hook :initform nil
	      :documentation "Called on application startup")
   (stop-app :initarg :stop :accessor stop-hook :initform nil
	      :documentation "Called on application shutdown")
   (system-timer :initarg :system-timer :accessor system-timer :initform nil
	       :documentation "Called when the system timer fires (events.lisp)")
   (login :initarg :login :accessor login-hook :initform nil
	  :documentation "Called when the site has an authenticated user")
   (logout :initarg :logout :accessor logout-hook :initform nil
	  :documentation "Called when the site is no longer authenticated")
   (change-language :initarg :change-language :accessor change-language-hook :initform nil
		    :documentation "Called when the language of the site changes")
   (test :initarg :test :accessor test-hook :initform nil))
  (:documentation "A set of application event hooks"))

(defvar *registry-hooks* (make-instance 'registry-hooks)
  "Global variable that holds application hooks")

(defmacro app-hook (type)
  `(slot-value *registry-hooks* (localize-symbol ,type 
						 :package (find-package :registry) 
						 :ignore-keywords nil)))

(defun add-hook (type name thunk)
  (when (find name (app-hook type) :key #'car)
    (remove-hook type name))
  (push (cons name thunk) 
	(app-hook type)))

(defmacro define-system-event-hook (name (type) &body thunk)
  "Primary interface for defining hooks into system events"
  (when (stringp (first thunk))
    (setf thunk (rest thunk)))
  `(eval-when (:load-toplevel :execute)
     (add-hook ,(as-keyword type)
	       ,(as-keyword name)
	       ,(cond ((and (= (length thunk) 1)
			    (symbolp (first thunk)))
		       `(quote ,(first thunk)))
		      ((eql (caar thunk) 'lambda)
		       (first thunk))
		      (t `(lambda ()
			    ,@thunk))))))

(defun remove-hook (type name)
  (let ((type (as-keyword type))
	(name (as-keyword name)))
    (setf (app-hook type) (delete name (app-hook type) :key #'car))))

(defun funcall-hook (type &rest args)
  (let ((type (as-keyword type)))
    (mapcar #'(lambda (rec)
		(apply 'safe-funcall (cdr rec) args))
	    (reverse (app-hook type)))))

(defun clear-hook (type)
  (let ((type (as-keyword type)))
    (setf (app-hook type) nil)))

(defun clear-hooks ()
  "Reset all the hooks"
  (setf *registry-hooks* (make-instance 'registry-hooks)))

(defun hook-names (type)
  (let ((type (as-keyword type)))
    (cars (app-hook type))))

(defun describe-hook (type)
  (let ((type (as-keyword type)))
    (print (hook-names type))))
