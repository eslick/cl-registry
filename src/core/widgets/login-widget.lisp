(in-package :registry)

;;
;; Front page login widget
;;

(defun make-front-login-widget ()
  (let ((login (make-instance 'front-login)))
    (setf (widget-continuation login)
	  (lambda (x)
	    (safe-funcall 'on-front-login x)))
    login))

(defwidget front-login ()
  ((username :accessor username 
	     :initform (make-instance 'string-presentation
				      :prompt "Username"
				      :query-name "username"))
   (password :accessor password
	     :initform (make-instance 'password-field-presentation
				      :prompt "Password"
				      :query-name "pw"))
   (message :accessor message :initform nil)))


(defmethod render-widget-body ((login front-login) &rest args)
  (declare (ignore args))
  (with-html-form (:post (make-action (lambda (&rest args)
					(apply 'handle-front-login login args))))
    (:h1 (str #!"Log In"))
    (render-prompt (username login)) (:br)
    (render-presentation-editable (username login)) (:br)
    (render-prompt (password login)) (:br)
    (render-presentation-editable (password login))(:br)
    (render-translated-button "Log In")
    (awhen (message login)
      (htm (:br) (:span :id "login-error" (str it))))
    (unless (get-site-config-param :login-self-register-disable)
      (htm
       (:p :id "login-new-user" (str #!"New User?") "&nbsp;"
	   (render-link "register" #!"Register Now >>"))))
    (:p :id "login-forgot" 
	(render-link (f* (setf (message login) nil)
			 (do-forgot-password-dialog)
			 (redirect "/" :defer :post-render))
		     #!"Forgot your username or password?"))))
    
(defun handle-front-login (login &key username pw forgot &allow-other-keys)
  (declare (ignore forgot))
  (multiple-value-bind (authenticated message)
      (authenticate-user username pw)
    (if authenticated
	(progn (set-session-user authenticated)
	       (funcall-hook :login authenticated)
	       (redirect-hook-to-login-target "/"))
	(setf (message login) message))))

