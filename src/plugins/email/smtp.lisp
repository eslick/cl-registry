(in-package :registry)

(defvar *inhibit-smtp* nil)
(defparameter *smtp-host* "smtp.media.mit.edu")
(defparameter *lamsight-reply-to* "lamsight-admin@media.mit.edu")

(defun send-email (addresses subject body)
;;  (if *inhibit-smtp*
;;    (log-message :email :debug "Inhibited mail with subject ~s to ~s" subject
;;		 addresses)
    (cl-smtp:send-email *smtp-host* 
			*lamsight-reply-to*
			addresses subject body))

(defparameter *enable-email-whitelist* nil)
(defparameter *user-whitelist* '("eslick" "rme" "clozure" "jaj"))

(defun whitelist-user-p (user)
  (member (username user) *user-whitelist* :test #'equal))

(defun send-email-to-users (users subject body)
  (let ((addresses (mapcar #'user-email 
			   (if *enable-email-whitelist*
			       (select-if #'whitelist-user-p (mklist users))
			       (mklist users)))))
    (when (and addresses (get-site-config-param :email-to-users-p))
      (loop for address in addresses
	   do (send-email address
			  subject
			  body)))))

  