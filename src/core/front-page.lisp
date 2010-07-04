(in-package :registry)

(registry-proclamations)

;; ==========================================
;;  Public page home content
;; ==========================================

;;
;; The current registry model has a single landing page with
;; configurable alternative content (about, tour, etc)
;;
;; The user can then login to access the internal site which
;; will be configured based on the site configuration file
;; specification of private-page plugins.
;;
;; Customizations are done entirely via the admin article editor
;;

(defun make-front-page ()
  (setup-registry-root-widget (root-composite))
  (let ((page (make-instance 'composite :widgets
			     (list (make-front-flash) ;; ISE: this doesn't work
				   (make-widget ;; this is for styling
				    (f* (with-html 
					  (:div :id "main-content-top"))))
				   (make-instance 'front-page-dispatcher))
			     :dom-id "main-content-area")))
    page))

(defun make-front-flash ()
  (make-instance 'flash
		 :messages (when (request-parameter "timeout")
			     (list #!"Your session has timed out, if you were using the site please log back in to continue working"))))
				  

;;
;; Public Front Page URL dispatching
;;

(defwidget front-page-dispatcher (dispatcher)
  ((main-page :accessor main-page :initarg :main-page
	      :initform (make-instance 'front-page))
   (content-pages :accessor content-pages :initform nil)))

(defmethod initialize-instance :after ((fp front-page-dispatcher) &rest args)
  (declare (ignore args))
  (let ((config (pairs (get-site-config-param :front-page))))
    (setf (content-pages fp) 
	  (assoc-get :content-pages config))))
    
(defmethod get-widget-for-tokens ((public front-page-dispatcher) uri-tokens)
  "The public page dispatches the incoming request, render myself if it's the 
   root or I return a tour page that matches the provided root token"
  (let ((tokens (remaining-tokens uri-tokens)))
    (acond ((and tokens (equal (first tokens) *api-root-url*))
	    (dispatch-api-handler (subseq (remaining-tokens uri-tokens) 1)))
	   ((and tokens (= (length tokens) 1)
		 (assoc (first tokens) (content-pages public) :test #'equal))
	    (pop-tokens uri-tokens 1)
	    (make-article-widget (cdr it) :render-title-p nil))
           ((get-unsubscribe-widget-from-uri-tokens uri-tokens)
            it)
	   ((dashboard-url-p)
	    nil)
	   ;; QualityMetric survey URLs
	   ;; Not using front page selector for now. This was a hack attempt to workaround a display glitch.
	   #+NIL
	   ((and tokens (equal (first tokens) "qualitymetric"))
	    (pop-tokens uri-tokens 1)
	    (or (weblocks::webapp-session-value :registry-qualitymetric-selector)
		;; Cache QualityMetric widget selector 
		(setf (weblocks::webapp-session-value :registry-qualitymetric-selector) (make-qualitymetric-selector))))
	   (t
	    (pop-tokens uri-tokens (length (remaining-tokens uri-tokens)))
	    (main-page public)))))

(defmethod dependencies append ((dispatcher front-page-dispatcher))
  (list (make-local-dependency :stylesheet "front")))

(defun get-unsubscribe-widget-from-uri-tokens (uri-tokens)
  (let ((tokens (remaining-tokens uri-tokens)))
    (when (and tokens
               (equal (first tokens) "unsubscribe"))
      (let ((type (second tokens))
            (username (third tokens))
            (salt (fourth tokens))
            (hash (fifth tokens)))
        (when (and (stringp type)
                   (stringp username)
                   (stringp salt)
                   (stringp hash)
                   (equal hash (unsubscribe-salt-xor-hash username salt)))
          (pop-tokens uri-tokens (length (remaining-tokens uri-tokens)))
          (make-email-unsubscribe-widget username type))))))

;;
;; Stylized home page layout widget - populated by articles content
;; 

(defwidget front-page ()
  ((main-pane :accessor main-pane-name :initform "public-home-main")
   (login-widget :accessor login-widget 
		 :initform (make-front-login-widget))
   (middle-pane :accessor middle-pane-name 
		:initform "public-home-middle")
   (bottom-pane :accessor bottom-pane-name
		:initform "public-home-bottom")))

(defmethod dependencies append ((page front-page))
  (list (make-local-dependency :stylesheet "front")))

(defmethod render-widget-body ((page front-page) &rest args)
  (declare (ignore args))
  (with-html
    (:div :class "main-pane"
	  (:div :class "main-pane-header" 
		(str (pane-content (main-pane-name page))))
	  (render-widget (login-widget page)))
    (:div :class "middle-pane"
	  (str (pane-content (middle-pane-name page))))
    (:div :class "bottom-pane"
	  (loop for article in (articles-for-pagename 
				(bottom-pane-name page)) do
	       (htm (:div :class "bottom-sub-pane"
			  (str (translated-content-html article))))))
    (:div :id "public-page-end")))

(defun pane-content (name)
  (handler-case 
      (translated-content-html
       (first
	(articles-for-pagename name)))
    (error () "")))

