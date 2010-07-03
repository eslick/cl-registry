(in-package :registry)

(defvar *inhibit-smtp* nil)

(defvar *smtp-host* nil)

(defparameter *lamsight-reply-to* "lamsight-admin@media.mit.edu")
(defparameter *ilr-reply-to* "admin@internationallamregistry.org")

(defun email-smtp-host ()
  (or *smtp-host*
      (setq *smtp-host* (get-site-config-param :email-smtp-host))
      (error "No site configuration parameter value for :EMAIL-SMTP-HOST")))

(defun site-email-admin-address ()
  "Email admin address to use in From header of email sent from Registry software to users"
  (cond
    ;; We should rely only on the email admin address set in site config
    ((get-site-config-param :email-admin-address))
    ;; We can fall back on known addresses for Registry applications
    ((aand (get-site-config-param :site-name)
	   (string-equal it "LAMsight"))
     *lamsight-reply-to*)
    ((aand (get-site-config-param :site-name)
	   (string-equal it "International LAM Registry"))
     *ilr-reply-to*)))

(defun send-email (addresses subject body)
  (cond
    (*inhibit-smtp*
     ;; Using Hunchentoot logging doesn't work outside of a request handler thread
     ;;(log-message :email :debug "Inhibited mail with subject ~s to ~s" subject
     ;;             addresses)
     nil)
    ((and (stringp addresses) (plusp (length addresses)))
     (cl-smtp:send-email (email-smtp-host)
			 (site-email-admin-address)
			 addresses subject body))))

(defparameter *enable-email-whitelist* nil)
(defparameter *user-whitelist* '("eslick" "kmcorbett" "wws" "rme" "clozure" "jaj"))

(defun whitelist-user-p (user)
  (member (username user) *user-whitelist* :test #'equal))

;; EMAIL-TO-USERS-P config param value is one of: NIL, T, a user class, or list of user classes.
;; Examples:
;;   (:email-to-users-p   nil)
;;   (:email-to-users-p   t)
;;   (:email-to-users-p   (:admins :owners))
;;
;; NIL means no email to users is allowed, T means email to all classes of users is allowed.
;; Specifying a user class means email to users of that class is allowed.
;; User class is a keyword representing a class of registry users such as :ADMINS, :OWNERS, or :USERS
;; 
;; See also WHEN-EMAIL-NOTIFICATION-ENABLED-P

(defun email-to-users-p (&optional user-class)
  "Is email enabled for users of class USER-CLASS? (a user class keyword symbol)"
  (let ((enabled (get-site-config-param :email-to-users-p)))
    (cond
      ((null enabled)			;no email allowed
       nil)
      ((eq enabled t))			;all users enabled
      ((null user-class)
       nil)
      ((not (symbolp (setq user-class (as-keyword user-class)))) ; who knows?
       nil)
      ((listp enabled)			;check the list
       (first (member user-class enabled :test #'string-equal)))
      ((eq (as-keyword enabled) user-class)) ;match the symbol
      )))

(defvar *enable-email-to-users-default-class* ':users)

(defun send-email-to-users (users subject body &key (user-class *enable-email-to-users-default-class*))
  (let ((addresses (mapcar #'user-email 
			   (if *enable-email-whitelist*
			       (select-if #'whitelist-user-p (mklist users))
			       (mklist users)))))
    (when (and addresses (email-to-users-p user-class))
      (loop for address in addresses
	   do (send-email address
			  subject
			  body)))))

  
