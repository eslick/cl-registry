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
		   (htm (:P "Signature:&nbsp;" (render-text-input "signature" "" :maxlength 64.))))
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
		   (htm (:P "Initials:&nbsp;" (render-text-input "initials" "" :maxlength 8.))))))))
	#'(lambda (&rest args)
	    (declare (ignore args))
	    (with-html
	      (:P (str (format nil "Error: article not found: ~A" article-name))))))))

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
		      (make-consent-form-widget study)))))

(defmethod render-widget-body ((widget study-list-item) &rest args)
  (declare (ignore args))
  (let ((patient (current-patient)))
    (with-slots (study patient-consent-form-article-widget) widget
      (setf (patient-consent-form-needed-p widget)
	    (and (requires-consent-p study)
		 (not (study-patient-consented-p study patient)))
	    ;; Only show patient consent form on redisplay for same patient
	    (patient-consent-form-visible-p widget)
	    (and (patient-consent-form-visible-p widget)
		 (eq (patient-consent-form-visible-p widget) patient)))
      (with-html
	(:DIV
	 :CLASS "study-list-item"
	 (:LI :CLASS "study-list-study-name" (str (name study))
	      (:P :CLASS "study-list-study-description" (str (description study)))
	      (let ((this-study-complete-p ':maybe)
		    (surveys
		     (loop for survey in (surveys study)
			when (include-study-survey-p study survey)
			collect survey)))
		(cond
		  ((null surveys)
		   (setq this-study-complete-p nil)
		   (htm
		    (:P :CLASS "study-list-message" "No surveys for study")))
		  ;; If study requires patient consent form *and* patient hasn't signed
		  ((patient-consent-form-needed-p widget)
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
		      (flet ((process-forms (&key signature agree disagree initials submit cancel)
			       (cond
				 ((or cancel (and submit disagree))
				  (setf (patient-consent-form-visible-p widget) nil))
				 ((not submit)) ;ignore this?
				 ((and agree (not disagree))
				  (if (and (stringp initials)
					   (>= (length (remove-if-not #'alpha-char-p initials)) 2))
				      (setf (study-patient-consented-p study patient)
					    (list signature initials (get-universal-time))
					    (patient-consent-form-visible-p widget) nil
					    (patient-consent-form-needed-p widget) nil)
				      (alert "Fill in your initials if you consent"))))
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
			(:P "Error! no article for consent form")))))
		  ;; Generate list of survey items
		  (t
		   (htm
		    (:UL
		     ;; Check survey rules and completion status, then display surveys
		     (let (next-survey
			   suppress-links-p)
		       (dolist (survey surveys)
			 (let ((this-survey-complete-p (survey-complete-p (current-patient) survey))
			       (enable-this-link-p (not suppress-links-p))
			       message
			       (survey-rule (find survey (survey-rules study) :key #'survey-rule-survey)))
			   ;; If survey not completed, then study not completed
			   (setq this-study-complete-p (and this-study-complete-p this-survey-complete-p))
			   ;; Display study list item for survey 
			   (htm
			    (:LI :CLASS "study-list-survey-item"
				 (:IMG :SRC
				       (format nil "/pub/images/surveys/~A.gif"
					       (if this-survey-complete-p "check" "checkbox")))
				 (str "&nbsp;")
				 (when survey-rule
				   (case (survey-rule-type survey-rule)
				     (:DOFIRST
				      (cond
					(this-survey-complete-p)
					((null next-survey)
					 (setq next-survey survey
					       suppress-links-p t
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
					     (:A :HREF (format nil "/dashboard/collect/~A/~A/" "survey" (mid survey))
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
		(when (and this-study-complete-p
			   (neq this-study-complete-p ':maybe))
		  (htm
		   (:P :CLASS "study-list-message"
		       (str #!"Surveys for this study are completed")))))))))))

;; Study-list widget contains one study-list-item for each study

(defwidget study-list (composite)
  ((compact-format-p :accessor compact-format-p :initarg :compact-format :initform nil)))

(defmethod render-widget-children ((widget study-list) &rest args)
  (declare (ignore args))
  nil)

(defvar _w)
(defun make-study-list (&key compact-format)
  (let ((widgets
	 (list
	  (make-instance 'study-list :compact-format compact-format
			 :widgets (make-study-list-items :compact-format compact-format)))))
    (if (get-site-config-param :survey-viewer-show-choose-patient-widget)
      (setq widgets (cons (make-choose-patient-widget :hr-p nil :mark-siblings-dirty-p t) widgets)))
    ;; Returns
    (setq _w
    (make-instance 'composite :widgets widgets))))

(defmethod render-widget-body ((widget study-list) &rest args)
  (declare (ignore args))
  (let ((study-list-item-widgets (composite-widgets widget)))
    (with-html
      (:DIV
       :CLASS "study-list"
       (:H2 (str #!"Studies"))
       (if (null study-list-item-widgets)
	   (htm (:P :CLASS "study-list-message" (str #!"No studies are defined")))
	   (htm
	    (:UL
	     (loop for child in study-list-item-widgets
		do (render-widget-body child)))))))))
