(in-package :registry)

(registry-proclamations)

(export '(plugin-not-available plugin-name define-plugin
	  registry-plugin site-app find-plugin plugin-loaded-p
	  create-plugin initialize-plugin))

;; ==============================
;; Conditions
;; ==============================

(define-condition plugin-not-available ()
  ((name :accessor plugin-name :initarg :name))
  (:report (lambda (condition stream)
	     (format stream "\"~A\" is not a registered plugin"
		     (plugin-name condition)))))

;; ==============================
;; Plugin Registration
;; ==============================

(defvar *plugins* (make-hash-table :test #'equalp))

(defclass registry-plugin ()
  ((name :accessor plugin-name :initarg :name)
   (init :accessor plugin-initialization :initarg :initialize :initform nil)
   (create :accessor plugin-create :initarg :create :initform nil)
   (loaded :accessor plugin-loaded-p :initform nil)))

(defclass site-app (registry-plugin)
  ((tab-name :accessor tab-name :initarg :tab-name)))

(defmethod register-plugin (name plugin)
  "A simple registration interface; associates a string and
   a symbol which is used to fetch the plugin object (typically
   a widget.  "
  (setf (gethash (as-keyword name) *plugins*) plugin))
  
(defun find-plugin (name)
  "Lookup the plugin"
  (let ((plugin (and name (gethash (as-keyword name) *plugins*))))
    (unless plugin
      (error 'plugin-not-available :name name))
    plugin))

;; ==============================
;; Plugin Resolution
;; ==============================

(defmethod initialize-plugin ((plugin registry-plugin))
  "Perform any initialization that needs to be run
   prior to using or creating an instance of the plugin code"
  (unless (plugin-loaded-p plugin)
    (safe-funcall (plugin-initialization plugin))))

(defmethod create-plugin-object ((plugin registry-plugin) args)
  "Make an object exported by the plugin; 
   signal an error if it doesn't exist"
  (safe-apply (plugin-create plugin) args))

;; =============================
;; Quick definition macros
;; =============================

(defmacro define-plugin (name type &rest args)
  "A plugin is a coherent body of functionality which can be used by a registry
   site-app or widget.

   Arguments:
   :initialize - defines a thunk which can load configuration parameters, etc.
   :create - defines a method for creating the plugin in the case where it is
             a widget or some other object the system manipulates.  Takes one
             argument which is a plist of keyword-value arguments."

  (with-gensyms (keyname)
    (when (stringp (first args))
      (setf args (rest args)))
    (let ((class (or (if (consp type) (first type) type) 'plugin)))
      `(eval-when (:load-toplevel)
	 (let ((,keyname (as-keyword ',name)))
	   (register-plugin ,keyname (make-instance ',class :name ,keyname ,@args)))))))
				       
