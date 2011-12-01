(in-package :registry)

(registry-proclamations)

;; =============================================
;;  Log-in Flows
;; =============================================

;; This permanent action provides a split-phase version of do-registry-login which
;; allows us to switch the root widget heirarchy and dispatch a specific URL.
;; The login flow can't run at an invalid URL, so we need to run the login flow,
;; then redirect to a special flow that sets up the hierarchy prior to rendering
;; it at the redirected URL.
;;
;; We use the request hook to avoid any issues with transactions in the login
;; authentication logic.

(defun/cc flow-got-lost ()
  (do-page (make-registry-dispatcher)) ;; attempt soft recovery
  (do-information "<p>The site has experienced a serious error twice ('The Flow was Lost').  Last time we returned you home.  This time we will log you out.  If you log back in and do not repeat what you were doing before, all should be well.</p>
<p>We would greatly appreciate if you would report what you were doing when this error occurred to lamsight-admin@media.mit.edu.</p>")
  (logout-handler))

(defun/cc do-login-dialog ()
  (do-dialog "" (make-login-widget)))

(define-permanent-action/cc login registry (&key redirect &allow-other-keys)
  (awhen (do-login-dialog)
    (if (eq it :forgot)
	(progn (do-forgot-password-dialog)
	       (redirect "/"))
	(redirect-hook-to-login-target redirect))))

(define-permanent-action/cc login-complete registry (&rest args)  
  (declare (ignorable args))
  (do-page (make-registry-dispatcher))
  (flow-got-lost))

(defun redirect-hook-to-login-target (target)
  (let ((url (or target "/dashboard/home/")))
    (post-action-redirect 
     (format nil "~A?action=login-complete" url))))

;;
;; Simple login flow
;;

(defun/cc do-registry-login ()
  (awhen (do-login-dialog)
    (if (eq it :forgot)
	(progn (when (do-forgot-password-dialog)
		 (redirect "/")))
	(do-page (make-registry-dispatcher)))
    (flow-got-lost)))


;; ====================================================
;;  Support function and view for login dialog
;; ====================================================

(defview login-view (:type form :persistp nil
		     :buttons '((:submit . "Login") :cancel)
		     :caption "Login"
		     :focusp t
		     :use-ajax-p t)
  (username :requiredp t)
  (password :requiredp t
	    :present-as password))

(defun login-handler (widget user-info)
  "Handler for the login widget"
  (declare (ignore widget))
  (multiple-value-bind (authenticated message)
      (authenticate-user-object user-info)
    (if authenticated 
	(progn
	  (funcall-hook :login authenticated)
	  (values authenticated nil))
	(values nil message))))

(defun make-login-widget (&optional (handler 'login-handler))
  (make-instance 'login
		 :view 'login-view
		 :on-login handler
		 :widget-suffix-fn 'forgot-password-link))


;; ==============================
;;  Logging out
;; ==============================

(defun logout-action (&rest args)
  (declare (ignore args))
  (logout-handler))

(defun logout-handler ()
  (funcall-hook :logout)
  (logout)
  (hunchentoot::remove-session hunchentoot:*session*)
  (redirect "/"))

;; ================================================================
;;  Authentication
;; ================================================================

(defstruct login-info username password)

(defun authenticate-user-object (user-info)
  (when user-info
    (authenticate-user (slot-value user-info 'username)
		       (slot-value user-info 'password))))

(defun authenticate-user (username pw)
  (let ((user (get-instance-by-value 'user 'username username)))
;;    (log-message :navigation :info "User login attempt: ~A" user)
    (if user
	(if (validate-sha1-password pw (user-password user))
	    (progn 
	      (log-message :navigation :info "User login success: ~A" user)
	      (unless (has-permission-p user :admin)
		(record-event :user-login user))
	      (values user nil))
	    (progn
	      (log-message :navigation :info "User ~a login attempt failed"
			   user)
	      (values nil "Invalid password")))
	(if (and username (not (equal username "")))
	    (values nil (format nil "Username '~A' is not recognized" username))
	    (values nil (format nil "No username provided"))))))

(defun session-user ()
  (authenticatedp))

(defun set-session-user (user)
  (setf (weblocks::webapp-session-value *authentication-key*) user))

;; ==============================================================
;;  SHA1 authentication (Compatable with default Django auth)
;; ==============================================================

(defun validate-sha1-password (raw-password pw-rec)
  (destructuring-bind (type salt orig-hex)
      (cl-ppcre:split "\\$" pw-rec)
    (declare (ignore type))
    (let ((new-hex (sha1-hash-password (concatenate 'string salt raw-password))))
      (equal orig-hex new-hex))))

(defun create-sha1-password (raw-password &optional salt)
  (let ((salt (or salt (make-salt))))
    (format nil "sha1$~A$~A" salt 
	    (sha1-hash-password (concatenate 'string salt raw-password)))))

(defun make-salt (&optional (length 5))
  (subseq (sha1-hash-password (random-string))
	  0 length))

(defun sha1-hash-password (string)
  (ironclad:byte-array-to-hex-string
   (ironclad:digest-sequence
    :sha1
    (ironclad:ascii-string-to-byte-array string))))

;; =============================================
;;  Forgotten passwords
;; =============================================

;;; Handle forgotten passwords

(defun forgot-password-link (login-widget &rest args)
  "Renders a link that tells the login flow that the user forgot their password"
  (declare (ignore args))
  (render-link (lambda/cc (&rest args)
		 (declare (ignore args))
		 (answer login-widget :forgot))
	       "Forgot your username or password?"))

(defparameter *forgot-password-sent-message*
  "Your password has been sent; after logging in don't forget to change your password")

(defparameter *problem-with-password-sent-message*
  "Your e-mail address was not recognized, or a server problem prevented us sending your password.  Please send a message to ILRHelp@lamtreatmentalliance.org if this problem persists.")

(defun/cc do-forgot-password-dialog ()
  (when (do-dialog "" (forgot-password-form))
    (do-information *forgot-password-sent-message*)
    (do-information *problem-with-password-sent-message*)))

(defun forgot-password-form ()
    (make-quickform 'forgot-password-view
     :answerp nil
     :satisfies (lambda (w data)
		  (valid-email-p w data))
     :on-success (lambda (w data)
		   (answer w (mail-new-password data)))
     :on-cancel (lambda (w)
		  (answer w nil))))

(defparameter *forgot-password-message*
  "If you can't remember your username or password, enter your email address here.  You will be sent a message that contains your username and a new password.")

(defview forgot-password-view (:type form :persistp nil
			       :buttons '((:submit . "Mail new password") :cancel)
			       :caption "Get New Password"
			       :default-fields-suffix-fn
			       (lambda (&rest args)
				 (declare (ignore args))
				 (with-html
				   (:p (str *forgot-password-message* ))))
			       :focusp t)
  (email :requiredp t))

(defvar *password-chars* "abcdefghijkmnopqrstuvwxyz123456789")

(defun generate-password (len)
  "Lame.  Very lame."
  (let* ((n (length *password-chars*))
	 (password (make-string len)))
    (dotimes (i len password)
      (setf (schar password i) (schar *password-chars* (random n))))))

(defun mail-new-password (data)
  (let* ((email (slot-value data 'email))
	 (user (get-instance-by-value 'user 'email email)))
    (when (and user (user-email user))	;paranoia
      (let ((new-password (generate-password 6)))
	(setf (user-password user) (create-sha1-password new-password))
	(send-email (user-email user)
		    #!"Password reset"
		    (funcall #'format nil #!"Hello.

The password for the username: ~a has been reset to ~a

After you log in, you can change the password to something you can remember.
"
				    (username user) new-password))
	t))))

(defun valid-email-p (w data)
  (let* ((email (string-trim-whitespace (slot-value data 'email)))
	 (user (get-instance-by-value 'user 'email email)))
    (if user 
	(user-email user)
	(let* ((view (find-view (dataform-form-view w)))
	       (field (first (view-fields view))))
	  (values nil (list (cons field #!"I cannot find that e-mail address.")))))))


