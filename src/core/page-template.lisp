(in-package :registry)

(registry-proclamations)

;;
;; The page template defines the overall framing, header, footer and feel of the site
;;
;;   Configurable components include:
;;   - The top header w/ logo and quick links nad language selection
;;   - Tab links for modules when logged in
;;   - [Default call to render widget hierarchy here]
;;   - Render the page footer (after main page renders)
;;


;; ===================================
;; Render the title 
;; ===================================

(defmethod application-page-title ((app registry))
  "Override the default title renderer, but reproduce most of it's fn's,
   but draw from config file instead of webapp definition"
  (let ((webapp-description (get-site-config-param :site-description)))
    (apply #'format nil "~A~A~A"
	   (get-site-config-param :site-name)
	   (cond
	     (*current-page-description* (list " - " *current-page-description*))
	     (webapp-description (list " - " webapp-description))
	     (t '("" ""))))))
  
;; ===================================
;; Render page header at top
;; ===================================

(defun setup-registry-root-widget (widget)
  (setf (widget-prefix-fn widget) 'render-header))

(defun render-header (&rest args)
  "This function renders the common page header and context."
  (declare (ignore args))
  (with-html
    (:div :class "header"
	  (:div :class "header-logo"
		(:a :href (if (authenticatedp) "/dashboard/home" "/")
		    (:img :src (get-site-config-param :header-logo-url))))
	  (:div :class "header-nav"
 			(:ul
			 (:li 
			  (render-autodropdown "language" (valid-languages)
					       #'change-language-handler
					       :use-ajax-p nil
					       :selected-value (session-language)))
			 (:li  (:a :href (help-mailto-string) (str #!"Help")) 
			       (str "&nbsp;|&nbsp;"))
			 (:li (render-registration-header))
			 (:li (render-login-header))))
	  (when (and (authenticatedp)
	    (render-header-menu)))
	  (:div :class "header-nav-bottom"))))

(defun help-mailto-string ()
  (format nil "mailto:~A?subject=~A&body=~A"
	  (get-site-config-param :email-admin-address)
	  #!"[LAMsight Help Request]"
	  #!"I'm having a problem with LAMsight and have provided details on my problem below."))

(defun render-login-header ()
  (aif (current-user)
       (with-html
	 (render-link #'logout-action 
		      (format nil "~A ~A (~A)" (find-translation "Welcome") (username it) (find-translation "logout")))
	 (str "&nbsp;|&nbsp;"))
       (with-html
	 (render-link "login" 
		      (format nil "~A" (find-translation "Login")))
	 (str "&nbsp;|&nbsp;"))))

(defun render-registration-header ()
  (unless (current-user)
    (with-html
      (render-link "register" (format nil "~A" (find-translation "Register")))
      (str "&nbsp;|&nbsp;"))))

;; ==============================
;; Render each module tab
;; ==============================

(defun render-header-menu ()
  (let* ((nav (get-registry-nav))
	 (tabs (cars (static-selector-panes nav)))
	 (selected (static-selector-current-pane nav)))
    (with-html
      (:div :id "header-tabs"
	    (:ul (loop for tab in tabs
		      do (render-tab tab selected)))))))

(defun render-tab (pane-name selected-pane)
  (with-html
    (:li :id (format nil "~A_tab" pane-name)
	 (:a :class (when (equal pane-name selected-pane) "current")
	     :href (format nil "/dashboard/~A/" pane-name)
	     (htm (:span :class "text-tabs" 
			 (str (humanize-name (translate pane-name)))))))))

(defun compute-selected-pane ()
  (declare (special *uri-tokens*))
  (if (= (length (remaining-tokens *uri-tokens*)) 1)
      "home"
      (second (remaining-tokens *uri-tokens*))))

;; ============================================================
;; And the footer is rendered after the site
;; ============================================================

(defmethod render-page-body :after ((app registry) rendered-html)
  "Render the common footer"
  (declare (ignore rendered-html))
  (with-html
    (:div :class "footer"
	  (:p (str (concatenate
		    'string 
		    (get-site-config-param :footer-copyright)))
	      "&nbsp; | &nbsp;"
	      (render-link (f* (do-dialog #!"Terms of Use"
				 (make-article-dialog-widget 
				  "terms-of-use" :ok nil)))
			   #!"Terms of Use")
	      "&nbsp; | &nbsp;"
	      (render-link (f* (do-dialog #!"Privacy Policy" 
				 (make-article-dialog-widget 
				  "privacy-policy" :ok nil)))
			   #!"Privacy Policy")
	      "&nbsp; | &nbsp;"
	      (:a :href (format nil "mailto:~A" (get-site-config-param :email-admin-address))
		  (str #!"Contact")))))
  (render-google-analytics))

(defun render-google-analytics ()
  (awhen (get-site-config-param :google-analytics-id)
    (with-html 
      (:script :type "text/javascript"
	       "var gaJsHost = ((\"https:\" == document.location.protocol) ? \"https://ssl.\" : \"http://www.\");
              document.write(unescape(\"%3Cscript src='\" + gaJsHost + \"google-analytics.com/ga.js' type='text/javascript'%3E%3C/script%3E\"));")
      (:script :type "text/javascript"
	       (str (format nil 
			    "try {
                                 var pageTracker = _gat._getTracker(\"~A\");
                                 pageTracker._trackPageview();
                               } catch(err) {}"
			    it))))))

(defun ajax-scroll-to-top ()
  (declare (special *on-ajax-complete-scripts*))
  (when (ajax-request-p)
    (send-script (ps:ps ((slot-value window 'scroll-to) 0 0)))))
				

