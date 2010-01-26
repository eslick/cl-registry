(in-package :registry)

;;
;; Build the top-level navigation environment
;;

(defun make-registry-dispatcher ()
  "Hack to get around limitations of current navigation 
   as pertains to making the navigation the parent of its panes"
  (let ((dispatcher (make-instance 'registry-dispatcher)))
    dispatcher))

(defwidget registry-dispatcher (dispatcher)
  ((home-page :accessor home-page :initform (make-registry-navigation))
   (content-pages :accessor content-pages :initform nil)))

(defmethod initialize-instance :after ((pd registry-dispatcher) &rest args)
  (declare (ignore args))
  (let ((config (pairs (get-site-config-param :registry-root))))
    (setf (content-pages pd) (assoc-get :content-pages config))))

(defmethod dependencies append ((dispatcher registry-dispatcher))
  (list (make-local-dependency :stylesheet "tour")))

;;(defmethod page-title ((widget registry-dispatcher))
;;  "LAMSIGHT2")

(defmethod get-widget-for-tokens ((dispatcher registry-dispatcher) uri-tokens)
  "This is entirely to add a dashboard token to the URI so we can differentiate
   whether we are a public or private URL during expired sessions and missing pages"
  (declare (special *request-hook*))
  (let ((tokens (remaining-tokens uri-tokens)))
    (acond ((and tokens (equal (first tokens) "crossdomain.xml"))
	    (hunchentoot:handle-static-file 
	     (make-pathname 
	      :name "crossdomain"
	      :type "xml"
	      :defaults (compute-webapp-public-files-path (current-webapp)))
	     "text/xml"))
	   ;; Render registry app
	   ((and tokens (equal (first tokens) "dashboard"))
	    (pop-tokens uri-tokens 1)
	    (home-page dispatcher))
	   ;; API Handler
	   ((and tokens (equal (first tokens) *api-root-url*))
	    (dispatch-api-handler (subseq (remaining-tokens uri-tokens) 1)))
	   ;; Generic content pages
	   ((and tokens (= (length tokens) 1)
		 (assoc (first tokens) (content-pages dispatcher) :test #'equal))
	    (pop-tokens uri-tokens (length tokens))
	    (make-article-widget (cdr it) :render-title-p nil :dom-id "tour"))
	   ;; Unsubscribe user URL
	   ((get-unsubscribe-widget-from-uri-tokens uri-tokens))
	   (t (home-page dispatcher)))))

;;
;; Construction the dashboard widget (move to widgets eventually)
;;

(defwidget registry-navigation (navigation)
  ()
  (:default-initargs :dom-id "dashboard"))

(defun set-registry-nav (nav)
  (setf (weblocks::webapp-session-value :registry-nav) nav))

(defun get-registry-nav ()
  (weblocks::webapp-session-value :registry-nav))

(defun make-registry-navigation ()
  (let ((nav (make-instance 'registry-navigation :name "dashboard" 
			    :dom-id "content")))
    (apply #'init-navigation nav (instantiate-site-apps))
    (set-registry-nav nav) ;; debugging
    (setf (widget-suffix-fn nav)
	  (f* (with-html (:div :id "dashboard-end"))))
    (make-instance 'composite :widgets 
		   (list 
		    (make-widget 
		     (f* (with-html (:div :id "dashboard-header"))))
		    nav))))

(defmethod render-navigation-menu ((nav registry-navigation) &rest args)
  (declare (ignore args)))

;;
;; Configuration driven registry nav instantiation
;;

(defun instantiate-site-apps ()
  (loop for config in (get-site-config-param :site-application-plugins)
     nconc (instantiate-site-app config)))

(defun instantiate-site-app (config)
  (setf config (mklist config))
  (let ((app (find-plugin (first config)))
	(args (rest config)))
    (assert (subtypep (type-of app) 'site-app))
    (awhen (getf args :permissions)
      ;;(print it)
      (unless (some (lambda (p) 
		      (has-permission-p (current-user) p t))
		    it)
	(return-from instantiate-site-app nil)))
    (list (tab-name app)
	  (create-plugin-object app (remove-keyword-parameters args :permissions)))))

;; 	   `(home ,home ;; (make-article-page "home")
;; 	     collect ,(make-survey-viewer) ;; (make-survey-viewer)
;; 	     explore ,(handler-case 
;; 		           (make-plugin (get-preference :explorer-plugin (current-user)))
;; 			 (plugin-not-available () (make-explorer)))
;; 	     discuss  ,(make-forums-page)
;; 	     create ,(make-survey-editor)
;; 	     ,@(when (or (is-editor-p) (is-admin-p)) (list 'admin (make-admin-page)))))
