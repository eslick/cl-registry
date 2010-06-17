(in-package :registry)

;; =============================================================================
;; Preferences Widget
;; =============================================================================

(defwidget user-preferences-widget (widget)
  ((preference-presentations :accessor preference-presentations
                             :initform (build-preference-presentations))
   (tabs :initarg :tabs :accessor preference-tabs :initform nil)
   (selected-tab :initform nil :accessor selected-preference-tab :initarg :selected)
   (prior-prefs :initform nil :accessor prior-rendered-preferences
		:affects-dirty-status-p nil)
   (refresh-on-close-p :initform nil :accessor refresh-on-close-p)))

(defmethod dependencies append ((prefs user-preferences-widget))
  (list (make-local-dependency :stylesheet "preferences")
	(make-local-dependency :script "preferences")))

(defun build-preference-presentations ()
  (let ((presentations (make-hash-table :test #'eql)))
    (maphash (lambda (preference-name user-preference)
               (let ((presentation (funcall (presentation-maker user-preference))))
                 (multiple-value-bind (value foundp)
                     (get-preference preference-name (current-user))
                   (when foundp
                     (setf (lisp-value presentation) value)))
                 (setf (gethash preference-name presentations) presentation)))
             *preferences*)
    presentations))

(defun make-preferences-page (&optional tab)
  (make-instance 'user-preferences-widget :selected tab))

(defmethod initialize-instance :after ((widget user-preferences-widget) &rest initargs)
  (declare (ignore initargs))
  (setf (preference-tabs widget) `((,#!"community" . user-identity-prefs-form)
                                   (,#!"personal" . personal-info-prefs-form)
                                   (,#!"contact" . contact-prefs-form)
                                   (,#!"site" . website-prefs-form)))
  (unless (selected-preference-tab widget) 
    (setf (selected-preference-tab widget) 
	  (car (first (preference-tabs widget))))))

(defmethod render-widget-body ((widget user-preferences-widget) &rest args)
  (declare (ignore args))
  (let ((*current-widget* widget))
    (declare (special *current-widget*))
    (setf (prior-rendered-preferences widget) nil)
    (with-html
;;      (:p (str #!"Please fill in the preferences for all the tabs"))
      (:div :id "preferences-tabs"
	    (:ul
	     (dolist (tabrec (preference-tabs widget))
	       (destructuring-bind (name . tab) tabrec
		 (declare (ignore tab))
		 (htm (:li (render-link (f* (setf (selected-preference-tab widget) name))
					(humanize-name (car tabrec))
					:class (if (equal (car tabrec) (selected-preference-tab widget))
						   "preference-tab preference-tab-selected"
						   "preference-tab"))))))))
      (:div :id "preferences"
	    (let ((rendering-fn (cdr (assoc (selected-preference-tab widget)
					    (preference-tabs widget)
					    :test #'equal))))
	      (with-html-form (:post (preference-form-handler widget)
				     :id "preferences-form")
		(:div :id "preferences-body"
		      (funcall rendering-fn))
		(:div :id "preferences-controls"
		      ;; When JavaScript is used to submit a form, it seems to
		      ;; make it so that the first submit button appears to have
		      ;; been pressed.  (That is, in form handler function, the
		      ;; foo keyword argument will be set.)
		      ;; We want to know when the user actually clicked a button,
		      ;; so I put in this gross kludge of rendering a button that
		      ;; is hidden.
		      (render-button "Foo" :class "hidden" :value #!"Foo")
		      (:br)
		      (render-translated-button "Done"))))))))

(defun render-pref-button (name &key (value (humanize-name name)) id (class "submit"))
  "Renders a button in a form; no JS"
  (with-html
    (:input :name (attributize-name name) :type "submit" :id id :class class
	    :value value 
	    :onclick "$('preferences').highlight('#707070');")))
		  
;; =============================================================================
;; Preference groups
;; =============================================================================

(defun user-identity-prefs-form ()
  (with-html
    (:div :class "preferences-user-identity-title"
	  (:h3 (str "What is your role on the site?")))
    (:div :class "preferences-user-identity"
	  (present-preferences 
           :lam-patient-p
           :tsc-patient-p
           :family-member-p
           :clinician-p
           :lam-researcher-p
           :researcher-p
	   :other-researcher-p
           :unknown-role-p))
    ;; LAMsight legacy compatibility
    (let ((sname (get-site-config-param :site-name)))
      (when (or (null sname) (string-equal sname "LAMsight"))
	(htm
	 (:div :class "preference-caretaker clear"
	       (:p (str #!"I am a family member or caretaker for another LAMsight user:"))
	       (present-preferences :family-member-patient)))))))

(defun simple-user-identity-prefs-form ()
  (with-html
    (:div :class "preferences-user-identity-title"
	  (:h3 (str #!"What is your role on the site?")))
    (:div :class "preferences-user-identity"
	  (present-preferences 
			       :lam-patient-p
			       :tsc-patient-p
                               :family-member-p
                               :clinician-p
                               :lam-researcher-p
                               :researcher-p
			       :other-researcher-p
                               :unknown-role-p))))

(defun personal-info-prefs-form ()
  (with-html
    (:div :class "preferences-personal-info"
	  (:h3 (str #!"Personal Information"))
	  (present-preferences 
			       :first-name
                               :last-name
                               :residence-addr-1
                               :residence-addr-2
                               :residence-city
                               :residence-prov
                               :residence-country
			       :postal-code
                               :home-phone
                               :work-phone))
    (let ((sname (get-site-config-param :site-name)))
      (when (or (null sname) (string-equal sname "LAMsight"))
	(htm
	 (:div :class "preferences-map-me"
	       (present-preferences :map-me-p)))))))

(defun contact-prefs-form ()
  (with-html
    (:div :class "preferences-email"
	  (:h3 (str #!"Forum Subscriptions"))
	  (present-preferences :forum-subscriber
			       :forum-email-frequency))
    (:div :class "preferences-email clear"
	  (:h3 (str #!"Site Updates"))
	  (present-preferences :update-subscriber
			       :update-email-frequency))
    (:div :class "preferences-contact clear"
	  (:h3 (str #!"The site administrators may contact me: "))
	  (present-preferences 
	   :contact-to-authenticate
	   :contact-for-study
	   :contact-for-data-verification)
	  (:p (present-preferences :contact-methods))
	  (:p :style "font-size: x-small; clear:both; margin-left: 30px;"
	      (str #!"Hold down the Ctrl or Alt key to select more than one answer")))))



(defun website-prefs-form ()
  (with-html
    (:div :class "preferences-website"
	  (:h3 (str #!"Website Preferences"))
	  (apply #'present-preferences 
		 (append '(:default-language)   
			 (when (is-admin-p) 
			   '(:enable-laura-p
			     :explorer-plugin)))))))
			   
	  

(defun make-role-dialog ()
  (make-instance 'registration-role-dialog))

(defwidget registration-role-dialog (user-preferences-widget)
  ((errorp :accessor reg-role-errorp :initform nil)))

(defmethod render-widget-body ((widget registration-role-dialog) &rest args)
  (declare (ignore args))
  (let ((*current-widget* widget))
    (declare (special *current-widget*))
    (setf (prior-rendered-preferences widget) nil)
    (with-html
      (:h1 (str
	    ;; LAMsight legacy compatibility 
	    (let ((sname (get-site-config-param :site-name)))
	      (if (or (null sname) (string-equal sname "LAMsight"))
		  #!"Complete your LAMsight registration"
		  #!"Complete your user registration"))))
      (when (reg-role-errorp widget)
	(htm (:p :style "color: red;"
                 (str #!"You must select a role to complete registration"))))
      (:div :class "preferences"
	    (with-html-form (:post (registration-preference-form-handler widget))
	      (:div :id "preferences-body"
		    (simple-user-identity-prefs-form))
	      (:div :id "preferences-controls"
		    (render-translated-button "Submit")))))))

(defun registration-preference-form-handler (widget)
  (lambda (&rest args)
    (set-user-preferences-from-widget widget args)
    (if (> (length args) 2)
	(answer widget)
	(setf (reg-role-errorp widget) t))))

(defun change-preference-value (preference old-value new-value)
  (loop for user in (all-users) do
       (when (equal (get-preference preference user) old-value)
	 (setf (get-preference preference user) new-value))))
