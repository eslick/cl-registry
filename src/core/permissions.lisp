(in-package :registry)

;; ==============================================================
;;  Permissions 
;; ==============================================================

(defvar *permissions* nil
  "Holds a list of permissions objects for fast searching")

(defpclass permission ()
  ((name :initarg :name :accessor name :index t)
   (description :initarg :description :accessor description))
  (:documentation "A permission represents an access privilege
    for a user account.  There is a protected-mixin for navigation
    classes which can require one or more permissions to access.
    This requires a login if the session is unauthenticated or
    redirects to the root if the user does not have the required
    permissions"))

(defmethod print-object ((permission permission) stream)
  (print-unreadable-object (permission stream :type t :identity t)
    (format stream "oid:~A ~S" (ele::oid permission) (name permission))))

(defmacro def-permission (name doc-string)
  (assert (stringp doc-string))
  (with-gensyms (new-permission)
    `(progn
       (let ((,new-permission
	      (progn
		(when *store-controller*
		  (make-permission ',name ,doc-string))
		(cons ',name ,doc-string))))
	 (pushnew ,new-permission *permissions* :test #'equalp)
	 (defvar ,(intern-format-pkg *package* "*~A-PERMISSION*" name)
	   ,new-permission
	   ,doc-string)
	 (defun ,(intern-format-pkg *package* "IS-~A-P" name) ()
	     (has-permission-p (current-user) ',name))))))

(defun make-permission (name description)
  (or (get-instance-by-value 'permission 'name name)
      (make-instance 'permission :name name
		     :description description)))

(defun create-permissions ()
  (loop for permission in *permissions* do
       (make-permission (first permission) (cdr permission))))

(def-permission admin
    "There is always an administrator and an administrator privilege")

;; ========================================
;;  Permissions API
;; ========================================

(defun get-permission (permission)
  (etypecase permission
    (symbol 
     (when (or (find permission *permissions* :key #'first)
	       (find (intern (symbol-name permission) :registry)
		     *permissions* :key #'first))
       (or (get-instance-by-value 'permission 'name permission)
           (progn
             (create-permissions)
             (get-instance-by-value 'permission 'name permission)))))
    (persistent-object 
     (when (find (name permission) *permissions* :key #'first)
       permission))))

(defun has-permission-p (user permission &optional specific-p)
  (unless (and user (user-permissions user))
    (return-from has-permission-p nil))
  (unless specific-p
    (when (find-item (get-permission 'admin) (user-permissions user))
      (return-from has-permission-p t)))
  (etypecase permission
    (persistent-object (find-item permission (user-permissions user)))
    (symbol (find-item (get-permission permission) (user-permissions user)))
    (cons (ecase (first permission)
	    (or (some (curry #'has-permission-p user) (rest permission)))
	    (and (every (curry #'has-permission-p user) (rest permission)))))))

(defun add-permission (user permission)
  (insert-item (get-permission permission) (user-permissions user)))

(defun remove-permission (user permission)
  (remove-item (get-permission permission) (user-permissions user)))

(defun clear-permissions (user) 
  (drop-slot-set (user-permissions user)))

(defun current-permission-p (permission)
  "Used in an action/render context to see if the current user has a permission"
  (has-permission-p (current-user) permission))

(defun permission-names (user)
  (let ((res nil)
        (permissions (user-permissions user)))
    (when permissions
      (map-slot-set (lambda (permission)
                      (push (string (name permission)) res))
                    (user-permissions user))
      (sort res #'string-lessp))))

(defun permission-names-string (user)
  (let ((perms (permission-names user)))
    (format nil "~@[~a~]~{ ~a~}" (car perms) (cdr perms))))

;; =====================================
;;  Default permission set
;; =====================================

(def-permission active-user
    "All registered users")

(def-permission authenticated-user
    "Users that have had their identity authenticated")

(def-permission authenticated-professional
    "An authenticated researcher, doctor or clinician")

(def-permission translator
    "Users that have asked to be translators, effects a home
     page widget")

(def-permission editor
    "A validated editor of site content")

(def-permission center-admin
    "An administrator of a center. Can add new clinicians to the center.")

(def-permission article-translator
    "Users that have been allowed to edit content in order to add article translations")

(defun content-editors ()
  (select-if (lambda (user)
	       (has-permission-p user :editor t))
	     (get-instances-by-class 'user)))
       

