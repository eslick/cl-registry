(in-package :registry)

(registry-proclamations)

;; =============================================================
;;  Study List View
;; =============================================================

;; Include study list item in study list view

(defun include-study-p (study)
  (or (published-p study)
      (is-admin-p)
      (eq (current-user) (owner study))))

;; Include survey list item in study list view

(defun include-study-survey-p (study survey)
  (declare (ignore study))
  (and (or (published-p survey)
	   (is-admin-p)
	   (eq (current-user) (owner survey))
	   (member (current-user) (survey-acl survey)))
       (not (edit-lock survey))))

;; Study-list-item displays surveys for study

(defwidget study-list-item ()
  ((study :accessor study :initarg :study :type study)
   (patient-consent-form-visible-p :accessor patient-consent-form-visible-p
				   :initarg :patient-consent-form-visible-p
				   :initform nil)
   (patient-consent-form-needed-p  :accessor patient-consent-form-needed-p
				   :initarg :patient-consent-form-needed-p
				   :initform t)
   (patient-consent-form-article-widget :accessor patient-consent-form-article-widget
					:initarg :patient-consent-form-article-widget
					:initform nil)
   (article-widget :accessor article-widget :initarg :article-widget :initform nil)
   (compact-format-p :accessor compact-format-p :initarg :compact-format :initform nil)))

(defmethod make-consent-form-widget ((study study) &rest args)
  (declare (ignore args))
  (let ((forms (patient-consent-forms study)))
    (cond
      ((null forms) nil)
      ((= (length forms) 1)
       (apply #'make-consent-form-widget (first forms)))
      (t
       (make-instance 'composite
		      :widgets
		      (loop for form in forms
			 collect (apply #'make-consent-form-widget form)))))))

(defmethod make-consent-form-widget ((article-name string) &rest specs)
  (let ((article (first (articles-for-pagename article-name))))
    (make-widget
     (if article
	 #'(lambda (&rest args)
	     (declare (ignore args))
	     (with-html
	       (:DIV :CLASS "study-list-consent-form-scroll-area"
		     (str (maybe-markdown
			   (slot-value-translation article 'content)
			   (article-content-type article))))
	       (dolist (spec specs)
		 (case spec
		   (:signature
		    ;; TODO: customizable HTML header (widget??) before / around signature block
		    (htm (:P "Subject Name:&nbsp;" (render-text-input "signature" "" :maxlength 64.))))
		   #|
		   (:date
		   (htm (:P "Date:&nbsp;mm/dd/yyyy")))
		   (:time
		   (htm (:P "Time:&nbsp;HH:MM:SS")))
		   |#
		  (:yes-no
		   (htm (:P (render-checkbox "agree" nil) (str #!"Yes")
			    (str "&nbsp;")
			    (render-checkbox "disagree" nil) (str #!"No"))))
		  (:initials
		   (htm (:P "Initial:&nbsp;" (render-text-input "initials" "" :maxlength 8.))))
		  (:yes-no-initials
		   (htm (:P "Please check one box and initial:&nbsp;"
			    (str #!"Yes") (render-checkbox "agree" nil)
			    (str "&nbsp;")
			    (str #!"No") (render-checkbox "disagree" nil)
			    (str "&nbsp;&nbsp;&nbsp;Initial:&nbsp;") (render-text-input "initials" "" :maxlength 8.))))))))
	#'(lambda (&rest args)
	    (declare (ignore args))
	    (with-html
	      (:P :CLASS "study-list-message"
		  (str (format nil "Error: article not found: ~A" article-name)))))))))

(defun make-study-list-items (&key compact-format)
  (let ((patient (current-patient)))
    (declare (ignore patient))
    (loop for study in (get-instances-by-class 'study)
       when (include-study-p study)
       collect
       (make-instance 'study-list-item :study study
		      :compact-format compact-format
		      :patient-consent-form-article-widget
		      ;; Create consent form article widget for study
		      (make-consent-form-widget study)
		      :article-widget
		      (aif (articles-page-name study)
			   (if (listp it)
			       nil
			       (make-instance 'quick-help :page it :link-title "More information")))))))

(defmethod render-widget-body ((widget study-list-item) &rest args)
  (declare (ignore args))
  (let ((patient (current-patient)))
    (with-slots (study patient-consent-form-article-widget) widget
      ;; ISE: Disable consent blocking
      (setf (patient-consent-form-needed-p widget)
	    (and (requires-consent-p study)
		 (not (study-patient-consented-p study patient))))
      ;; Only show patient consent form on redisplay for same patient
      (setf (patient-consent-form-visible-p widget)
	    (and (eq (patient-consent-form-visible-p widget) patient) patient))
      (with-html
	(:DIV
	 :CLASS "study-list-item"
	 (:LI :CLASS "study-list-study-name" (str (name study))
	      (:P :CLASS "study-list-study-description" 
		  (str (description study))
		  (awhen (articles-page-name study)
		    (htm 
		     (render-link (f* (do-dialog "" 
					(make-article-widget 
					 it 
					 :render-answer-p t)))
				  "&nbsp; [Read study protocol...]"))))
	      (let ((this-study-complete-p ':maybe)
		    (surveys
		     (loop for survey in (surveys study)
			when (include-study-survey-p study survey)
			collect survey)))
		(cond
;;		  ((null surveys)
;;		   (setq this-study-complete-p nil)
;;		   (htm
;;		    (:P :CLASS "study-list-message" "No surveys for study")))
		  ;; If study requires patient consent form *and* patient hasn't signed
		  ((and (eq (requires-consent-p study) :estrogen)
			(not (study-patient-consented-p study patient)))
		   (htm 
			"Thank you for your interest in the LAM Estrogen Study.  We have reached full enrollment and are no longer accepting new registrations.  Once the Study has concluded we will post a summary of the results on LAMsight.  We are very grateful for your willingness to participate and hope that you will continue to volunteer for future studies."))
;;		    (render-link "estrogen-study-signup" "Register for Study" :class "register-link")))
		  ((and (eq (requires-consent-p study) :estrogen)
			(study-patient-consented-p study patient)
			(not (estrogen-study-activated-p)))
		   (htm 
		    "Thank you for your interest in the LAM Estrogen Study.  We have reached full enrollment and are no longer accepting new registrations.  Once the Study has concluded we will post a summary of the results on LAMsight.  We are very grateful for your willingness to participate and hope that you will continue to volunteer for future studies."
			;; "Thank you for your interest in the LAM Dyspnea and Hormone Study.  Your registration request is being processed and a staff member will be contacting you by email with further instructions.  The recruiting period for this study ends on September 3rd, 2012.  If you have not heard from us within 2 weeks of this date or have any questions please contact us at: <a href=\"mailto:EstrogenStudy@LAMTreatmentAlliance.org\">EstrogenStudy@LAMTreatmentAlliance.org</a>"
			))
		  ((and (patient-consent-form-needed-p widget)
			(neq (requires-consent-p study) :estrogen))
		   ;; Display link to consent form or the form itself
		   (cond
		     ;; Consent form not visible?
		     ((not (patient-consent-form-visible-p widget))
		      ;; Provide link to display consent form
		      (render-link
		       (f* (setf (patient-consent-form-visible-p widget) patient)
			   (mark-dirty (widget-parent widget)))
		       #!"Show consent form"))
		     (patient-consent-form-article-widget
		      ;; Display submit/cancel form
		      (flet ((process-forms (&rest args
						   &key signature agree disagree initials submit cancel
						   &allow-other-keys)
			       (declare (ignore args))
			       (cond
				 ((or cancel (not submit))
				  ;; Hide consent forms
				  (setf (patient-consent-form-visible-p widget) nil)
				  (mark-dirty (widget-parent widget)))
				 ((and agree disagree)
				  (alert "Select Yes or No only"))
				 ((and (or agree disagree) (not (stringp initials)))
				  (alert "Internal error - initials not valid"))
				 ((and (or agree disagree) (< (length (remove-if-not #'alpha-char-p initials)) 2.))
				  (alert "Fill in your initials"))
				 ;; Validate signature
				 ((not (stringp signature)) (alert "Internal error - signature not valid"))
				 ((<= (length (remove-if-not #'alpha-char-p signature)) 3.)
				  (alert "Fill in your signature if you consent"))
				 ;; Valid online consent
				 (t
				  (setf (study-patient-consented-p study patient)
					`(:consent-p t
					  :signature
					  (,signature
					   ,@(and (stringp initials) (>= (length initials) 2)
						  `(,(if agree :ilr-data-use-agree :ilr-data-use-disagree) ,initials))))
					(patient-consent-form-visible-p widget) nil
					(patient-consent-form-needed-p widget) nil)))
			       (mark-dirty (widget-parent widget))))
			(with-html-form (:post #'process-forms)
			  ;; Display consent form content
			  (render-widget patient-consent-form-article-widget :inlinep t)
			  (with-html
			    (:P (render-translated-button "Submit")
				(str "&nbsp;")
				(render-translated-button "Cancel"))))))
		     (t
		      ;; No article?
		      (with-html
			(:P :CLASS "study-list-message" "Error: no article for consent form")))))
		  ;; Generate list of survey items
		  (t
		   (htm
			(:p :style "margin-left: 1.5em; color: #303030; font-size: small; line-height: 120%;"
				"Thank you for your participation in the LAM Estrogen Study.  It is now time for you to begin collecting measurements and filling out the two daily diaries, the Estrogen Study Diary and the Dyspnea Diary, which can be found below.  Please contact us if you have any questions at: " (:a :href "mailto:EstrogenStudy@LAMTreatmentAlliance.com" "EstrogenStudy@LAMTreatmentAlliance.com") " Thank you!")
		    (:b "Study Surveys and Diaries")
		    (:UL
		     :CLASS "study-list-survey-list"
		     ;; Check survey rules and completion status, then display surveys
		     (let (next-survey
			   suppress-links-p)
		       (dolist (survey surveys)
			 (let ((this-survey-complete-p (survey-complete-p (current-patient) survey))
			       (enable-this-link-p (not suppress-links-p))
			       message
			       (survey-rule (find survey (survey-rules study) :key #'survey-rule-survey))
			       survey-url)
			   ;; If survey not completed, then study not completed
			   (setq this-study-complete-p (and this-study-complete-p this-survey-complete-p))
			   ;; Display study list item for survey 
			   (htm
			    (:LI :CLASS "study-list-survey-item-ls"
;; NOTE: This hack screws up ILR checkboxes on completion
;;				 (:IMG :SRC
;;				       (format nil "/pub/images/surveys/~A.gif"
;;					       (if this-survey-complete-p "check" "checkbox")))
;;				 (str "&nbsp;")
				 (when survey-rule
				   ;; Get survey URL - may be link to external survey
				   (setq survey-url (survey-rule-url survey-rule))
				   ;; State changes depending on survey rule type
				   (case (survey-rule-type survey-rule)
				     (:DOFIRST
				      (cond
					(this-survey-complete-p)
					((null next-survey)
					 (setq next-survey survey
					       suppress-links-p nil ;; ISE: Disable link suppression
					       message #!"Complete this survey first"))))
				     (:REQUIRED
				      (cond
					(this-survey-complete-p)
					(suppress-links-p)
					((null next-survey)
					 (setq next-survey survey
					       message #!"Complete this survey next"))
					(t
					 (setq message #!"This survey is required"))))
				     (:OPTIONAL)))
				 (if enable-this-link-p
				     (htm
				      (:SPAN :CLASS "study-list-survey-name"
					     (:A :HREF
						 (or survey-url
						     (format nil "/dashboard/collect/~A/~A/" "survey" (mid survey)))
						 (:SPAN :CLASS "study-list-survey-name-alink"
							(str (name survey)))
						 (:SPAN :CLASS "study-list-survey-description-alink"
							(str (description survey))))))
				     (htm
				      (:SPAN :CLASS "study-list-survey-name"
					     (str (name survey)))))
				 (cond
				   ((null message))
				   ((compact-format-p widget)
				    (htm
				     (:SPAN
				      :CLASS "study-list-message-small"
				      (:IMG :SRC "/pub/images/surveys/arrow-left.gif")
				      (str "&nbsp;")
				      (str message))))
				   (t
				    (htm
				     (:BR)
				     (:SPAN
				      :CLASS "study-list-message"
				      (:IMG :SRC "/pub/images/surveys/arrow-up.gif")
				      (str "&nbsp;")
				      (str message))))))))))))))
		;; Check completed status
		;; TBD: check study completed status at beginning and display at top
;;		(aif (article-widget widget)
;;		     (htm (:P (render-widget it))))
		(when (and this-study-complete-p
			   (neq this-study-complete-p ':maybe))
		  (htm
		   (:P :CLASS "study-list-message"
		       (str #!"Surveys for this study are completed")))
		  (aif (study-complete-message study)
		       (htm
			(:P :CLASS "study-list-message"
			    (str it))))))
	      (when (and (eq (requires-consent-p study) :estrogen)
			 (study-patient-consented-p study patient))
		(htm 
		 (:P :style "margin-left: 1.5em; font-size: small;"
		     (unless (and (> (length (get-preference :residence-addr-1 (current-user))) 0)
				  (> (length (get-preference :residence-city (current-user))) 0)
				  (> (length (get-preference :postal-code (current-user))) 0))
		       (render-link (f* (do-dialog #!"User Preferences" (make-preferences-page #!"personal")))
				    "Please Complete Your Contact Information")))
		 (:P :style "margin-left: 1.5em; font-size: small;"
		     (render-link (f* (do-dialog #!"Estrogen Study Contact Preferences" (make-estrogen-preferences-dialog)))
				  "Manage reminder preferences"))
		 (:P :style "margin-left: 1.5em; font-size: small;"
		     (render-link (f* (withdraw-from-estrogen-study (current-user)))
				  "Withdraw from Study"))
		 ))))))))
				      


;; Study-list widget contains one study-list-item for each study

(defwidget study-list (composite)
  ((compact-format-p :accessor compact-format-p :initarg :compact-format :initform nil)))

(defmethod render-widget-children ((widget study-list) &rest args)
  (declare (ignore args))
  nil)

(defun make-study-list (&key compact-format)
  (let ((widgets
	 (list
	  (make-widget (f* (render-survey-list-header :study)))
	  (make-instance 'study-list :compact-format compact-format
				     :widgets (make-study-list-items :compact-format compact-format)))))
    (if (get-site-config-param :survey-viewer-show-choose-patient-widget)
	(setq widgets (cons (make-choose-patient-widget :hr-p nil :mark-siblings-dirty-p t) widgets)))
    ;; Returns
    (make-instance 'composite :widgets widgets)))

(defmethod render-widget-body ((widget study-list) &rest args)
  (declare (ignore args))
  (let ((study-list-item-widgets (composite-widgets widget)))
    (with-html
      (:DIV
       :CLASS "study-list"
       (cond
	 ((null (current-patient))
	  (htm (:P :CLASS "study-list-message"
		   (str #!"You do not have access to any studies"))))
	 ((null study-list-item-widgets)
	  (htm (:P :CLASS "study-list-message" (str #!"No studies are defined"))))
	 (t
	  (htm
	   (:OL :CLASS "study-ulist"
		(loop for child in study-list-item-widgets
		   do (render-widget-body child))))))))))
