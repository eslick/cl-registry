(in-package :registry)

;; ================================
;; Site Configuration Files
;; ================================

(defun read-startup-configuration (config)
  "You can provide multiple config files in a list.  They are loaded in the list order
   and higher configs will shadow lower configs.  This allows a developer
   to take a standard site config and customize it for the development environment"
  (cond ((null config)
	 (error "A site configuration is required to start the registry"))
	((stringp config)
	 (read-site-configuration config))
	((listp config)
	 (mapcar #'read-site-configuration config))
	(t (error "Can't load configuration from ~A" config))))

(defvar *site-configuration* nil
  "Initialize this from a file to define how the site is configured and 
   to provide site-specific parameters")

(defun get-site-config-table ()
  (aif-ret *site-configuration*
    (setf *site-configuration* 
	  (make-hash-table))))

(defun read-site-configuration (filename)
  "Read a single file into the site-configuration hash"
  (let ((site-config 
	 (with-open-file (stream filename)
	   (read stream)))
	(config (get-site-config-table)))
    (loop for entry in site-config do
	 (setf (gethash (car entry) config) 
	       (if (equal (length (cdr entry)) 1) 
		   (cadr entry)
		   (cdr entry))))
    (hash-keys config)))

(defun reset-site-configuration (&rest files)
  (setf *site-configuration* nil)
  (mapc #'read-site-configuration files)
  (hash-keys *site-configuration*))

(defun get-site-config-param (name)
  (gethash name *site-configuration*))

(defun (setf get-site-config-param) (value name)
  (setf (gethash name *site-configuration*) value))


;; ================================
;; Plugin features and capabilities
;; ================================

(defvar *lamsight-features* nil
  "Register any add-ons that we only want to init once, 
   or test for their presence")



