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
;; Render custom header entries
;; ===================================

(defmethod render-page-headers ((app registry))
  (awhen (get-site-config-param :google-analytics-id)
    (with-html
      (:script :type "text/javascript"
	       (str (format nil 
              "var _gaq = _gaq || [];
              _gaq.push(['_setAccount', '~A']);
              _gaq.push(['_trackPageview']);

              (function() {
                 var ga = document.createElement('script');
                 ga.src = ('https:' == document.location.protocol ? 'https://ssl' : 
                           'http://www') + '.google-analytics.com/ga.js';
                           ga.setAttribute('async', 'true');
                           document.documentElement.firstChild.appendChild(ga);
                 })();" it))))))

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
		    (let ((logo-url (get-site-config-param :header-logo-url))
			  (logo-text (get-site-config-param :header-logo-text)))
		      (if logo-url
			  (htm (:img :src logo-url)))
		      (when logo-text
			(if (eq logo-text ':site-name)
			    (setq logo-text (get-site-config-param :site-name)))
			(htm (:span :class "header-logo-text" (str logo-text)))))))
	  (:div :class "header-nav"
 			(:ul
			 (unless (get-site-config-param :header-languages-menu-hide)
			   (htm
			    (:li 
			     (render-autodropdown "language" (valid-languages)
						  #'change-language-handler
						  :use-ajax-p nil
						  :selected-value (session-language)))))
			 (:li  (:a :href (help-mailto-string) 
				   :onclick "_gaq.push(['_trackEvent','help','header']);"
				   (if (eq (get-portal-name) ':lamsight)
				       (str #!"Help")
				       (str #!"Support")))
			       (str "&nbsp;|&nbsp;"))
			 (:li (render-registration-header))
			 (:li (render-login-header))))
	  (when (and (authenticatedp)
	    (render-header-menu)))
	  (:div :class "header-nav-bottom"))))

(defun help-mailto-string ()
  (let* ((portal (get-portal-name))
	 (ilr-p (eq portal ':ilr))
	 (lamsight-p (eq portal ':lamsight)))
    (format nil "mailto:~A?subject=~A&body=~A"
	  (get-site-config-param :email-admin-address)
	  (cond
	    (lamsight-p #!"[LAMsight Help Request]")
	    (ilr-p "[ILR Help Request]")
	    (t "[Registry Help Request]"))
	  (cond
	    (lamsight-p
	     #!"I'm having a problem with LAMsight and have provided details on my problem below.")
	    (ilr-p
	     "I'm having a problem with ILR and have provided details on my problem below.")
	    (t
	     "I'm having a problem with the Registry ~@[~A~]and have provided details on my problem below."
	     (get-site-config-param :site-name))))))

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
  (unless (or (current-user) (get-site-config-param :login-self-register-disable))
    (with-html
      (render-link "register" (format nil "~A" (find-translation "Register")))
      (str "&nbsp;|&nbsp;"))))

;; ==============================
;; Render each module tab
;; ==============================

(defun render-header-menu ()
  (let* ((nav
	  (cond
	    ((get-registry-nav))
	    ((and (make-registry-navigation) ;2nd chance if flow got lost!!
		  (get-registry-nav)))))
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
;; Main content area is between page headers and footer
;; WITH-MAIN-CONTENT-AREA-HTML generates DIV tags w/ style IDs
;; ============================================================

(defmacro with-main-content-area-html (&body body)
  ;; Check for syntax errors
  (dolist (spec body)
    (unless (and (listp spec)
		 (typep (first spec) '(or string symbol))
		 (member (first spec) '(:top :middle-top :middle-indent :middle-bottom :bottom) :test #'string-equal))
      (error "Invalid specification for WITH-MAIN-CONTENT-AREA-HTML: ~S" spec)))
  (let ((top (rest (assoc :top body :test #'string-equal)))
	(middle-top (rest (assoc :middle-top body :test #'string-equal)))
	(middle-indent (rest (assoc :middle-indent body :test #'string-equal)))
	(middle-bottom (rest (assoc :middle-bottom body :test #'string-equal)))
	(bottom (rest (assoc :bottom body :test #'string-equal))))
    `(with-html
       (:DIV :ID "main-content-area"
	     (:DIV :ID "main-content-top" ,@top)
	     (:DIV :ID "main-content-middle" ,@middle-top
		   (:DIV :ID "main-content-middle-indent" ,@middle-indent)
		   ,@middle-bottom)
	     (:DIV :ID "main-content-bottom" ,@bottom)))))
		       
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
		  (str #!"Contact"))))))

(defun ajax-scroll-to-top ()
  (declare (special *on-ajax-complete-scripts*))
  (when (ajax-request-p)
    (send-script (ps:ps ((slot-value window 'scroll-to) 0 0)))))
				

