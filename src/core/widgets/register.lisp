(in-package :registry)

(registry-proclamations)

;;
;; Registration permanent handler
;;
;; Invokes a flow to register or to confirm a registration
;;
;; NOTE: Reg widget assumes old problematic flow control; 
;;       should clean this up eventually to match continuation
;;       based flows

(define-permanent-action/cc register registry (&key magic &allow-other-keys)
  (if magic
      (destructuring-bind (message &optional valid) 
		  (mklist (confirm-registration magic))
		(let ((valid-p valid)
			  (reg (find-registration magic)))
		  (do-information message)
		  (if valid-p 
			  (progn
				(make-registered-user reg)
				(record-event :registration-success (reg-username reg) 
							  :user (get-user (reg-username reg)))
				;;	    (set-session-user (get-user (reg-username reg)))
				;;	    (do-dialog "" (make-role-dialog))
				(set-preference-defaults (get-user (reg-username reg)))
				(finalize-registration reg)
				(hunchentoot::remove-session hunchentoot:*session*)
				(set-session-user nil)
				(redirect "/dashboard"))
			  (progn
				(redirect "/")))))
      (progn
		(awhen (do-dialog "" (make-instance 'registration))
		  (registration-form-accept it))
		(redirect "/"))))

;;
;; We can hack the update problem for now by registering a post-render action
;; which requests a refresh and then removes itself...
;;


;;
;; New registration widget
;;

(defparameter *registration-valid-days* 120)

(defwidget registration (composite)
  ((quickform :accessor registration-quickform :initarg :quickform :initform nil)
   (flash :accessor registration-flash :initarg :flash
	  :initform (make-instance 'flash))
   (pending-p :accessor pending-p :initform nil)
   (view :accessor registration-view :initarg :view :initform 'registration-view)))

(defview registration-view (:type form :persistp nil
			    :caption #!"Submit Request"
			    :buttons '((:submit . "Submit")
				       (:cancel . "Cancel"))
			    :use-ajax-p t)
  (username :requiredp t)
  (password :present-as password :requiredp t)
  (confirm-password :present-as password :requiredp t)
  (email :requiredp t))

(defmethod initialize-instance :after ((r registration) &rest initargs)
  (declare (ignore initargs))
  (setf (registration-quickform r)
	(make-quickform (registration-view r)
			:satisfies (lambda (w data)
				     (declare (ignore w))
				     (valid-registration-p data))
			:answerp t))
  (setf (composite-widgets r)
	(list (registration-flash r)
	      (registration-quickform r))))

(defun/cc registration-form-accept (data)
  (if (and (eq (do-dialog "" (development-status-notice)) :accept)
	   (eq (do-dialog "Privacy Policy" (accept-privacy-policy)) :accept)
	   (eq (do-dialog "Terms of Use" (accept-terms-of-use)) :accept))
      (progn (submit-registration data)
	     (record-event :registration-sent (slot-value data 'username) :user nil)
	     (do-information #!"Thank you for registering.  A message containing a confirmation link and instructions has been sent to your email addresss."))
      (do-information (format nil #!"We're sorry to hear that you are not ready to register.  Questions about our privacy policy or terms of use can be sent to ~A.  We are evolving these documents and hope to address any concerns you might have in time; your input is valuable.  Thank you." (get-site-config-param :email-admin-address)))))

(defun development-status-notice ()
  (make-articles-dialog "development-status-notice"))

(defun accept-privacy-policy ()
  (make-articles-dialog "privacy-policy"))

(defun accept-terms-of-use ()
  (make-articles-dialog "terms-of-use"))

(defun make-articles-dialog (page &optional (choices '(:accept :decline)))
  (let ((composite (make-instance 'composite)))
    (setf (composite-widgets composite)
	  (list (make-article-widget page :standalone-p (not (ajax-request-p)))
		(lambda ()
		  (with-html
		    (weblocks::render-choices-post "" choices 
						   (widget-continuation composite))))))
    composite))


(defmethod render-widget-body ((r registration) &rest args)
  (declare (ignore args))
  (render-widget (registration-flash r))
  (if (pending-p r)
      (with-html
		(:h1 (str #!"Thanks for registering"))
		(:p (str #!"Thanks for registering.  A message containing a
confirmation link has been sent to your email address.")))
      (progn
		(with-html
		  (:div 
		   (:h1 (str #!"Registration Instructions"))
		   (:p (str #!"A username is a short name that will be used by
               the system to identify you to other users.  If you are
               concerned about privacy, please pick a username that 
               doesn't include anything that identifies you.  A 
               password should be at least 6 characters and include 
               a number or other non-alphabetical character."))
		   (:p (str #!"To register, fill in the form below and click the \"Submit\" button, then read and scroll to the bottom of each of the three subsequent screens, and, if you agree to the conditions, click \"Accept\" on each page.  You will then receive an e-mail with instructions for completing your registration.")))))))

(defun valid-registration-p (data)
  (flet ((find-field-for-slot-name (slot-name fields)
	   ;; This is a bit of a mess.  We have to return an
	   ;; a-list of fields and error messagses, but it's not
	   ;; as convenient as one would like to get one's hands
	   ;; on the field objects.
	   ;; fields: a list of field-info structures
	   ;; f: the field slot in said structure
	   ;; view-field-slot-name: slot in f (field object)
	   (field-info-field
	    (find slot-name fields
		  :key (lambda (f)
			 (let ((field (field-info-field f)))
			   (view-field-slot-name field)))))))
    (let* ((fields (get-object-view-fields data 'registration-view))
		   (username-field (find-field-for-slot-name 'username fields))
		   (password-field (find-field-for-slot-name 'password fields))
		   (confirm-password-field
			(find-field-for-slot-name 'confirm-password fields))
		   (email-field (find-field-for-slot-name 'email fields))
		   (alist nil))
      (with-slots (username email password confirm-password) data
		(when (get-instances-by-value 'user 'username username)
		  (push (cons username-field
					  (format nil "username ~a already taken" username))
				alist))
		(when (get-instances-by-value 'user 'email email)
		  (push (cons email-field
					  (format nil "email ~a already taken: <a href=\"/login\">forgot username or password?</a>" email))
				alist))
		(when (not (equal password confirm-password))
		  (push (cons password-field #!"passwords must match") alist)
		  (push (cons confirm-password-field #!"passwords must match") alist))
		(if alist
			(values nil alist)
			t)))))

(defun make-registration-url (request &key (host "www.lamsight.org"))
  (format nil "http://~A/?action=register&magic=~A"
	  host
	  (reg-magic-key request)))

(defun make-registration-message-body (request)
  (format nil "
Hello.

Your LAMsight registration confirmation link is below.  Please
click on it to complete your registration.

~a

If you didn't register as a LAMsight user, please ignore this
message.

" (make-registration-url request)))

(defun/cc submit-registration (data)
  (format t "Submitting registration")
  (with-slots (username email password) data
    (let ((request (make-instance 'registration-request
				  :username username
				  :password password
				  :email email
				  :date (get-universal-time)
				  :magic-key (weblocks::generate-action-code))))
      (persist-object *default-store* request)
      (send-registration-request request))))

(defun send-registration-request (req)
  (send-email (mklist (reg-email req))
			  (if (string-equal (get-site-config-param :site-name) "LAMsight")
				  #!"LAMsight Registration Confirmation"
				  #!"Registration Confirmation")
			  (make-registration-message-body req)))

(defun confirm-registration (magic)
  ;; self-registration disabled
  ;; took too long -> re-register
  ;; already registered
  ;; successful -> login page
  (if (get-site-config-param :login-self-register-disable)
      (format nil "User self-registration is disabled. Please contact the site administrators")
   (let* ((reg (find-registration magic)))
    (cond ((or (not reg) (registration-timeout-p reg))
	   #!"Your registration request was not found.  Please try registering again.")
	  ((already-registered-p reg)
	   (format nil "User ~a is already registered" 
		   (reg-username reg)))
	  ((valid-registration-key-p reg magic)
	   (let ((username (reg-username reg)))
	     (list
	      (format nil "Hi '~A', welcome to LAMsight!  Press OK to continue." username)
	      t)))
	  (t
	   (if (string-equal (get-site-config-param :site-name) "LAMsight")
	       #!"Invalid registration.  Please contact the LAMsight administrators"
	       #!"Invalid registration.  Please contact the site administrators"))))))


;;
;; Utilities
;;

(defun find-registration (magic)
  (find magic (find-persistent-objects *default-store* 'registration-request)
	:key #'reg-magic-key :test #'equal))
				  
(defun already-registered-p (request)
  (when (get-instance-by-value 'user 'username (reg-username request)) t))

(defun registration-timeout-p (request)
  (when (> (- (get-universal-time) (reg-date request))
	   (* 3600 24 *registration-valid-days*))
    t))

(defun valid-registration-key-p (request magic)
  (equal magic (reg-magic-key request)))

(defun make-registered-user (request)
  (user-add (reg-username request) (reg-pw request)
	    :email (reg-email request)))

(defun finalize-registration (request)
  (delete-persistent-object *default-store* request))


