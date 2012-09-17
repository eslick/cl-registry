
(in-package :registry)

(define-plugin admin (site-app)
  "The create function accepts a list of plugins"
  :tab-name 'admin
  :create 'make-admin-page)

(defun make-admin-page (&rest args)
  (declare (ignore args))
  (make-instance 
   'composite 
   :dom-id "admin-page"
   :widgets
   (list (apply #'make-navigation 
	  "Admin Page"
	  `(,@(when (is-admin-p)
		    (list #!"Stats" (make-stats-dashboard)))
	    ,@(when (is-admin-p)
		    (list #!"Tools" (make-admin-dashboard)))
            ,@(when (or (is-admin-p) (is-editor-p))
		    (list #!"Contact Users" (make-contact-widget)
                          #!"Content Editor" (make-content-widget)))
	    ,@(when (is-admin-p)
		    (list #!"Model Editor" (make-admin-widget))))))))


(defun make-stats-dashboard ()
  (make-instance 'stats-dashboard))

(defwidget stats-dashboard ()
  ())

(defmethod render-widget-body ((admin stats-dashboard) &rest initargs)
  (declare (ignore initargs))
  (render-stats)
  (with-html
    (:br)
    "Email to users: "
    (str (if (get-site-config-param :email-to-users-p) "Enabled" "Disabled"))
    (render-link
     (f* (toggle-site-config-param :email-to-users-p)
         (mark-dirty admin))
     " [Toggle]")
    (:br)
    "Twitter updates: "
    (str (if (get-site-config-param :twitter-enabled-p) "Enabled" "Disabled"))
    (render-link
     (f* (toggle-site-config-param :twitter-enabled-p)
         (mark-dirty admin))
     " [Toggle]")
    (:br)
    "Analytics" 
    (:a :onclick "pageTracker._setVar('no_report');" 
	"Ignore My Browser")
    (:br)
    (flet ((process-dropdowns (&key error-action log-level &allow-other-keys)
             (setf *error-action*
                   (cond ((equal error-action "HTML") :html)
                         ((equal error-action "DEBUG") :debug)
                         (t nil)))
             (set-log-level
              (cond ((equal log-level "ERROR") :error)
                    ((equal log-level "WARNING") :warning)
                    ((equal log-level "INFO") :info)
                    ((equal log-level "DEBUG") :debug)))))
      (with-html-form (:get #'process-dropdowns :use-ajax-p t :class "autodropdown")
        "Error Action: "
        (render-dropdown "error-action"
                         '(("Log" . nil)
                           ("Display" . :html)
                           ("Debugger" . :debug))
                         :selected-value *error-action*
                         :autosubmitp t)
        (:br)
        "Log Level: "
        (render-dropdown "log-level"
                         '(("Error" . :error)
                           ("Warning" . :warning)
                           ("Info" . :info)
                           ("Debug" . :debug))
                         :selected-value (or *log-level* :error)
                         :autosubmitp t)))))


(defun make-admin-dashboard ()
  (make-instance 'admin-dashboard))

(defwidget admin-dashboard ()
  ())

(defparameter *archive-models* 
  '(center clinician patient
    study survey survey-group
    question answer answer-history
    consent-history))

(defun dump-filename (dir)
  (format nil "~A/data-archive-~A.sexp"
	  dir
	  (format-time-string nil "%Y%m%d%H%M%S")))

(defun dump-data-models (directory)
  "Not for Archival Purposes.  
   Dumps all data models to a file in directory."
  (export-models-to-file (dump-filename directory)
			 :model-list *archive-models*))

(defmethod render-widget-body ((admin admin-dashboard) &rest initargs)
  (declare (ignore initargs))
  (with-html
    (render-link (f* (dump-data-models "/var/backup"))
		 "Dump the database")
    "to /var/backup"
    ;; Login as another user
    (:br)
    (render-link
     (f* (do-dialog "Select a user" (make-user-login)))
     "Log-in as another user")
    ;; Add a new user
    (:div
     (:b "Add a new user account")
     (with-html-form (:get (lambda (&rest rest)
			     (apply 'add-new-user-handler rest)))
       "First" (render-text-input :first nil :id "first")
       "Last" (render-text-input :last nil :id "last") (:br)
       "E-mail" (render-text-input :email nil :id "email") (:br)
       "Username" (render-text-input :username nil :id "username")
       "Password" (render-text-input :password nil :id "password")
       (render-translated-button "create")))
    ;; Set a user's password
    (:div 
     (:b "Set a user's password as plaintext")
     (with-html-form (:get (lambda (&rest rest)
			     (apply 'reset-user-password rest)))
       "Username" (render-text-input :username nil :id "username")
       "Password" (render-password :newpass nil :id "newpass")
       (render-translated-button "update")))
	;; Change username
	(:div
	 (:b "Change a username")
	 (with-html-form (:get (lambda (&rest rest)
							 (apply 'change-user-username rest)))
	   "Old Username" (render-text-input :username1 nil :id "username1")
	   "New Username" (render-text-input :username2 nil :id "username2")
       (render-translated-button "update")))
    ;; Edit user permissions
    (let ((user (session-edit-permissions-user)))
      (with-html
	(:div
	 (:b "Change Permissions") (:br))
	(with-html-form (:get (lambda (&rest rest
				       &key name change update &allow-other-keys)
				(cond (change
				       (setf (session-edit-permissions-user)
					     (get-user name)))
				      (update
				       (let ((user (session-edit-permissions-user)))
					 (when user
					   (if (getf rest :admin)
					       (add-permission user :admin)
					       (dolist (perm.desc *permissions*)
						 (let ((perm (car perm.desc)))
						   (if (getf rest (keyword perm))
						       (add-permission user perm)
						       (remove-permission user perm)))))))
				       (setf (session-edit-permissions-user) nil))
				      (t (setf (session-edit-permissions-user) nil)))
				(mark-dirty admin))
			      :use-ajax-p t)
	  (cond (user
		 (htm "User: " (str (username user)) (:br))
		 (dolist (perm (sort (mapcar 'car *permissions*)
				     (lambda (x y)
				       (if (eq x 'admin) t
					   (string-lessp x y)))))
		   (render-checkbox (string perm) (has-permission-p user perm t))
		   (htm " " (str perm) (:br)))
		 (render-translated-button "update")
		 (render-translated-button "cancel"))                                  
		(t (htm "User: "
			(:input :type "text" :name "name" :size 20)
			(render-translated-button "change")))))))
    ;; Activate user for estrogen study
    (with-html
      (:div
       (:b "Activate user for estrogen study")
       (with-html-form (:get (lambda (&rest rest
				      &key uname-or-email &allow-other-keys)
			       (aif (get-user uname-or-email)
				    (progn
				      (activate-estrogen-study it)
				      (do-information "Successful activation"))
				    (do-information "User not found"))
			       (redirect "/dashboard/admin/tools")))
	 (htm "Username or E-mail:"
	      (:input :type "text" :name "uname-or-email" :size 20)
	      (render-translated-button "activate")))))))

(defun reset-user-password (&rest rest &key username newpass &allow-other-keys)
  (declare (ignorable rest))
  (aif (get-user username)
       (if (> (length newpass) 2)
	   (progn (setf (user-password it) 
			(create-sha1-password newpass))
		  (alert "Password successfully set"))
	   (alert "Password too short"))
       (alert (format nil "Username '~A' not found" username))))

(defun change-user-username (&rest rest &key username1 username2 &allow-other-keys)
  (declare (ignorable rest))
  (aif (get-user username2)
	   (alert (format nil "User with username '~A' already exists" username2))
	   (aif (get-user username1)
			(if (> (length username2) 2)
				(progn (setf (username it) username2)
					   (alert "Username changed"))
				(alert (format nil "New username '~A' empty or too short" username2)))
			(alert (format nil "Username '~A' not found" username1)))))

(defun add-new-user-handler (&rest rest &key username password first last email &allow-other-keys)
  (declare (ignorable rest))
  (if (every (lambda (string)
	       (and string (> (length string) 1)))
	     (list username password first last email))
      (progn (user-add username password :first first :last last :email email)
	     (alert (format nil "Added ~A ~A with username '~A' to the ILR"
			    first last username)))
      (alert "All fields are required and must be >= 2 characters")))

(defparameter *edit-permissions-user-key* 'edit-permissions-user)

(defun session-edit-permissions-user ()
  (webapp-session-value *edit-permissions-user-key*))

(defun (setf session-edit-permissions-user) (user)
  (setf (webapp-session-value *edit-permissions-user-key*) user))

(defun make-user-login ()
  (lambda (k)
    (with-html
      (mapc (lambda (user)
	      (render-link (lambda (&rest args)
			     (declare (ignore args))
			     (set-session-user user)
			     (answer k)
			     (post-action-redirect "/dashboard/admin"))
			   (username user))
	      (htm "&nbsp;"))
	    (all-users)))))



(defun render-stats ()
  (with-html
    (:div :class "composite"
	  (:p (print-user-stats *weblocks-output-stream*))
	  (:p (:ul 
	       (mapcar #'(lambda (user)
			   (htm (:li (str (username user)))))
		       (authenticated-server-users)))))))

;;	  'guide-test (make-instance 
;; 		       'composite
;; 		       :widgets 
;; 		       (list 
;; 			(make-instance 'guide-viewer
;; 				       :guide (first (get-instances-by-class 'guide)))))

(defun make-contact-widget ()
  (make-instance 'contact-widget ))

(defwidget contact-widget ()
  ())

(defun session-email-user ()
  (webapp-session-value 'email-user))

(defun (setf session-email-user) (user)
  (setf (webapp-session-value 'email-user) user))

(defun session-email-state ()
  (webapp-session-value 'email-state))

(defun (setf session-email-state) (state)
  (setf (webapp-session-value 'email-state) state))

(defun user-namestring (user)
  (let ((first-name (get-preference :first-name user))
        (last-name (get-preference :last-name user)))
    (cond ((blankp first-name)
           (if (blankp last-name)
               nil
               last-name))
          ((blankp last-name) first-name)
          (t (format nil "~a ~a" first-name last-name)))))

;; Pop up a dialog of users whose username or namestring contains SUBSTRING.
;; Call setter with the selected user or NIL.
(defun get-user-from-substring (substring &optional setter)
  (let (users)
    (map-class (lambda (user)
                 (when (some (lambda (str)
                               (search substring str
                                       :test #'char-equal))
                             (list (username user)
                                   (user-namestring user)))
                   (push user users)))
               'user)
    (setf users (sort users #'string-lessp :key #'username))
    (cond ((null (cdr users))
           (funcall setter (car users)))
          (t (do-dialog "Select a user"
               (lambda (k)
                 (mapc (lambda (user)
                         (render-link (lambda (&rest args)
                                        (declare (ignore args))
                                        (answer k)
                                        (funcall setter user))
                                      (format nil "[~a~@[ (~a)~]] "
                                              (username user)
                                              (user-namestring user))))
                       users)
                 (with-html
                   (:br)
                   (render-link (lambda (&rest args)
                                  (declare (ignore args))
                                  (answer k)
                                  (funcall setter nil))
                                "Cancel"))))))))
                                  

(defun blankp (x)
  (or (null x) (equal x "")))

(defun user-formatted-email (user)
  "Returns two values, \"name\" <email> and name"
  (let* ((name (user-namestring user))
         (email (user-email user)))
    (values (if (blankp name)
                email
                (format nil "~s <~a>" name email))
            name)))

(defun alert (message &key (title "Alert") thunk)
  (do-dialog title
    (lambda (k)
      (with-html-form (:get
                       (lambda (&rest rest)
                         (declare (ignore rest))
                         (answer k)
                         (when thunk (funcall thunk)))
                       :use-ajax-p t)
        (str message)
        (:br)
        (render-translated-button "OK" nil)))))

(defun/cc handle-email-form (widget &rest args &key name contact contact-all-users contact-lam-patients contact-estrogen-study contact-test-users cctome from subject message send
                          &allow-other-keys)
  (block nil
    (cond (contact
           (return (get-user-from-substring
                    name
                    (lambda (user)
                      (setf (session-email-user) user)
                      (mark-dirty widget)))))
	  (contact-all-users
	   (setf (session-email-user) :all)
	   (mark-dirty widget))
	  (contact-lam-patients
	   (setf (session-email-user) :patients)
	   (mark-dirty widget))
	  (contact-estrogen-study
	   (setf (session-email-user) :estrogen)
	   (mark-dirty widget))
	  (contact-test-users
	   (setf (session-email-user) :test)
	   (mark-dirty widget))
          (send
           (setf (session-email-state)
                 (list cctome from subject message))
           (let ((user (session-email-user)))
             (when user
               (flet ((do-alert (message)
                        (alert message
                               :thunk
                               (lambda ()
                                 (mark-dirty widget)))
                        (return)))
                 (cond ((blankp subject)
                        (do-alert "The subject is missing!"))
                       ((blankp message)
                        (do-alert "No message body provided!"))
		       (t
			(handler-case
			    (if (keywordp user)
				(when (eq (do-choice "Are you sure you want to email the entire group?" '(:yes :no)) :yes)
				  (send-email-to-group user from subject message))
				(send-email-to-users user subject message :from from))
			  (error (c)
			    (do-alert
				(format nil "~a" c)))))))
	       (setf (session-email-user) nil
		     (session-email-state) nil))))
          (t (setf (session-email-user) nil
                   (session-email-state) nil)))
    (mark-dirty widget)))

(defmethod render-widget-body ((widget contact-widget) &rest initargs)
  (declare (ignore initargs))
  (let ((user (session-email-user))
        (state (session-email-state)))
    (with-html
      (unless user
        (htm
         (:div "Contact a single user: " (:br) (:tiny "Enter a substring of the user id, first name, or last name"))))
      (with-html-form (:get (curry #'handle-email-form widget)
                            :use-ajax-p t)
        (cond ((not (or (typep user 'user) (member user (valid-email-groups))))
               (htm "User: "
                    (:input :type "text" :name "name" :size 20 :autocomplete "off")
		    "&nbsp;"
                    (render-translated-button "contact")
		    (:br)
		    (:br)
		    (render-translated-button "contact all users")
		    (render-translated-button "contact lam patients")
		    (render-translated-button "contact estrogen study")
		    (render-translated-button "contact test users")))
              (t
	       (cond 
		 ((typep user 'user)
		  (render-user-contact-form user state))
		 ((member user (valid-email-groups))
		  (htm
		   (:table
		    (render-email-form user nil state)))))))))))


(defun render-user-contact-form (user state)
  (let* ((addr1 (get-preference :residence-addr-1 user))
		 (addr2 (get-preference :residence-addr-2 user))
		 (city (get-preference :residence-city user))
		 (prov (get-preference :residence-prov user))
		 (country (get-preference :residence-country user))
		 (zip (get-preference :postal-code user))
		 (home-phone (get-preference :home-phone user))
		 (work-phone (get-preference :work-phone user))
		 (to-authenticate (get-preference :contact-to-authenticate user))
		 (for-study (get-preference :contact-for-study user))
		 (for-verify (get-preference :contact-for-data-verification user))
		 (methods (get-preference :contact-methods user))
		 (email (user-email user))
		 (email-p (member "email" methods :test #'equal))
		 (phone-p (member "phone" methods :test #'equal))
		 (mail-p (member "mail" methods :test #'equal))
		 (other-patient-p (member "another patient" methods :test #'equal))
		 (from-email (user-formatted-email (current-user))))
    (multiple-value-bind (to-email name) (user-formatted-email user)
      (with-html 
		(htm "User: " (str (username user)) (:br))
		(htm
		 (:p
		  (cond ((or to-authenticate for-study for-verify)
				 (htm "Approves communication ")
				 (when to-authenticate
				   (htm "to confirm identity")
				   (when (or for-study for-verify)
					 (htm (str (if (and for-study for-verify) ", " " or ")))))
				 (when for-study
				   (htm "for a potential study")
				   (when for-verify
					 (htm (str (if to-authenticate ", or " " or ")))))
				 (when for-verify
				   (htm "to follow up and verify data"))
				 (htm (:br)))
				(t (htm "Does not allow any communication." (:br))))
		  (unless mail-p
			(htm "Does " (:b "not ") "want to be contacted by mail."
				 (:br)))
		  (unless phone-p
			(htm "Does " (:b "not ") "want to be contacted by phone."
				 (:br)))
		  (unless other-patient-p
			(htm "Does " (:b "not ") "want to be contacted through another patient."
				 (:br)))
		  (unless email-p
			(htm "Does " (:b "not ") "want to be contacted by email."
				 (:br)))))
		(htm
		 (:table
		  (unless (blankp name)
			(htm
			 (:tr
			  (:td "Name:")
			  (:td (esc name)))))
		  (unless (every #'blankp
						 (list addr1 addr2 city prov zip country))
			(htm
			 (:tr
			  (:td :valign "top" "Address:")
			  (:td
			   (unless (blankp addr1)
				 (htm (esc addr1) (:br)))
			   (unless (blankp addr2)
				 (htm (esc addr2) (:br)))
			   (unless (and (blankp city) (blankp prov) (blankp zip))
				 (unless (and (blankp city) (blankp prov))
				   (htm (esc city)
						(unless (blankp prov)
						  (htm ", ")))
				   (unless (blankp prov)
					 (htm (esc prov)
						  (unless (blankp zip)
							(htm " ")))))
				 (htm (esc zip) (:br)))
			   (unless (blankp country)
				 (htm (esc country)))))))
		  (unless (blankp home-phone)
			(htm
			 (:tr
			  (:td "Home Phone:")
			  (:td (esc home-phone)))))
		  (htm (:tr
				(:td "Email:")
				(:td (esc email))))
		  (unless (blankp work-phone)
			(htm
			 (:tr
			  (:td "Work Phone:")
			  (:td (esc work-phone)))))
		  (render-email-form user from-email state)))))))

(defun render-email-form (user from-email state)
  (with-html
    (:tr
     (:td (:b "Send Email")))
    (:tr
     (:td "From:")
     (:td (:select :name "from"
		   (:option :value "LAMsightHelp@lamtreatmentalliance.org"
			    "LAMsight Admin (Help)")
		   (:option :value "EstrogenStudy@lamtreatmentalliance.org"
			    "Estrogen Study Admin")
		   (:option :value from-email "Your account"))))
    (typecase user
      (keyword (htm (:tr 
		     (:td "To") 
		     (:td (str (case user
				 (:all "All Users")
				 (:patients "All Patients")
				 (:estrogen "Estrogen Study Members")
				 (:test "Test Group")))))))
      (user (htm (:tr
		  (:td (str "To User:"))
		  (:td (esc (username user)))))))
    (:tr
     (:td "CC to you:")
     (:td (render-checkbox "cctome" (if state (first state) t))))
    (:tr
     (:td "Subject:")
     (:td (:input :type "text" :name "subject" :size 40
		  :value (third state))))
    (:tr
     (:td "Message:")
     (:td (render-textarea "message" (or (fourth state) "") 20 60)))
    (:tr
     (:td "&nbsp;")
     (:td (render-translated-button "send")
	  (render-translated-button "cancel")))))
    

