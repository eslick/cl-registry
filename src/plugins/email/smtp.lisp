(in-package :registry)

(defvar *inhibit-smtp* nil)

(defvar *smtp-host* nil)

(defvar *smtp-authentication* '(:unknown))

(defparameter *lamsight-reply-to* "LAMsightHelp@lamtreatmentalliance.org")
(defparameter *ilr-reply-to* "ILRHelp@lamtreatmentalliance.org")

(defun site-email-smtp-host ()
  (or *smtp-host*
      (setq *smtp-host* (get-site-config-param :email-smtp-host))
      (error "No site configuration parameter value for :EMAIL-SMTP-HOST")))

(defun site-email-admin-address ()
  "Email admin address to use in From header of email sent from Registry software to users"
  (let ((portal (get-portal-name :default nil)))
    (cond
      ;; We should rely only on the email admin address set in site config
      ((get-site-config-param :email-admin-address))
      ;; We can fall back on known addresses for Registry applications
      ((eq portal ':lamsight) *lamsight-reply-to*)
      ((eq portal ':ilr) *ilr-reply-to*)
      (t (error "No site configuration parameter value for :EMAIL-ADMIN-ADDRESS")))))

(defun site-email-smtp-authentication ()
  (check-type *smtp-authentication* cons)
  (case (first *smtp-authentication*)
    (:unknown
     (setq *smtp-authentication*
	   (or (get-site-config-param :email-smtp-authentication)
	       '(:none)))
     nil)
    (:none nil)
    (otherwise *smtp-authentication*)))
      
(defun safe-smtp-send (host from to subject message &rest args)
  (handler-bind ((cl-smtp:rcpt-failed 
		  #'(lambda (c)
		      (warn "Could not send e-mail to: '~A'~%" 
			    (cl-smtp::recipient c))
		      (invoke-restart 'cl-smtp::ignore-recipient))))
    (apply #'cl-smtp:send-email host from to subject message args)))

(defun send-email (addresses subject body &optional from)
  (cond
    (*inhibit-smtp*
     ;; Using Hunchentoot logging doesn't work outside of a request handler thread
     ;;(log-message :email :debug "Inhibited mail with subject ~s to ~s" subject
     ;;             addresses)
     nil)
    ((and (listp addresses) (plusp (length addresses)))
     (format t "Sending email to: ~A~%" addresses)
     (safe-smtp-send (site-email-smtp-host)
		     (or from (site-email-admin-address))
		     addresses subject body
		     :authentication (site-email-smtp-authentication))
     t)
    (t (format t "Unable to send e-mail to ~A, not a list?~%" addresses)
	   nil)))

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

(defun allowed-to-email-p (mode user)
  "Check whether we have permission to email this user for a given 
   type of outreach :forums, :updates, :recruit, nil for general"
  (and (or (not *enable-email-whitelist*)
	   (whitelist-user-p user))
       (not (get-preference :never-contact user))
       (let ((methods (get-preference :contact-methods user)))
	 (and (listp methods)
	      (member "email" methods :test #'equal)))
       (or (not (eq mode :forums))
	   (get-preference :forum-subscriber user))
       (or (not (eq mode :updates))
	   (get-preference :update-subscriber user))
       (or (not (eq mode :recruit))
	   (get-preference :contact-for-study user))))

(defun send-email-to-users (users subject body &key (user-class *enable-email-to-users-default-class*) from type)
  (let ((addresses (mapcar #'user-email 
			   (select-if (curry 'allowed-to-email-p type)
				      (mklist users)))))
    (when (and addresses (email-to-users-p user-class))
      (loop for address in addresses
	   do (send-email (mklist address)
			  subject
			  body
			  from)))))

;; Different group mailing mechanism than the user-class thing above

(defun lookup-email-group (groupname)
  (case groupname
    (:all (all-users))
    (:test (list (get-user "ianeslick") (get-user "eslick")
		 (get-user "Amanda")))
    (:estrogen (select-if #'(lambda (u)
			      (has-preference-value-p u :estrogen-study t))
			  (all-users)))

    (:patients
     (select-if (f (p) (has-preference-value-p p :lam-patient-p t))
		(all-users)))))

(defun valid-email-groups ()
  '(:all :test :estrogen :patients))

(defun send-email-to-group (groupname from subject body)
  (bordeaux-threads:make-thread 
   (f () (send-email-to-group* groupname from subject body))
   :name "Send Group Email"))

(defun send-email-to-group* (groupname from subject body)
  (let* ((users (select-if (curry #'allowed-to-email-p nil)
			   (lookup-email-group groupname)))
	 (groups (group users 100)))
    (dolist (group groups)
      (safe-smtp-send (site-email-smtp-host)
		      (or from (site-email-admin-address))
		      (list (or from (site-email-admin-address)))
		      subject body
		      :bcc (mapcar #'user-email group)
		      :authentication (site-email-smtp-authentication)))))
