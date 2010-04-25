
(in-package :registry)

(registry-proclamations)

;;
;; Export as site application
;;

(define-plugin clinician-home (site-app)
  "Defines a clinician home page and accepts a list of plugins to put into the side bar"
  :tab-name 'home
  :create 'make-clinician-home-page)

;;
;; Patient home page
;;

(defwidget clinician-home (composite)
  ((workspace :accessor home-page-workspace :initarg :workspace 
	      :initform nil)
   (map :accessor home-page-map :initarg :map)))

(defmethod render-widget-children ((home clinician-home) &rest args)
  (declare (ignore args))
  nil)

(defmethod render-widget-body ((widget clinician-home) &rest args)
  (declare (ignore args))
  (with-html
    (:div :class "home-main-pane"
	  (render-widget (first (composite-widgets widget))))
    (:div :class "home-sidebar"
	  (:div :class "top"
		;; Home page content on the right followed by plugin widgets
		(render-widget (second (composite-widgets widget)))
		(mapcar #'render-widget (rest (rest (composite-widgets widget)))))
	  (:div :class "bottom"
		(str "&nbsp;")))))

(defun make-clinician-home-page (&key plugins &allow-other-keys)
  (initialize-current-patient)
  (labels ((render-rule-for-widget (&rest args)
	     (declare (ignore args))
	     (with-html (:hr)))
	   (set-widget-rule-between (widget &key before after)
	     (if before
		 (setf (widget-prefix-fn widget) #'render-rule-for-widget))
	     (if after
		 (setf (widget-suffix-fn widget) #'render-rule-for-widget))
	     ;; Returns
	     widget))
    (let ((home
	   (make-instance 'clinician-home 
			  :widgets
			  `(,(make-instance 'composite
					    :widgets
					    (list (make-widget 'no-javascript)
						  (make-choose-center-widget)
						  (make-choose-patient-widget)
						  (set-widget-rule-between (make-patient-editor-widget) :after t)
						  (set-widget-rule-between (make-clinician-editor-widget) :after t)
						  (make-center-editor-widget)))
			     ,(make-article-widget "clinician-home" :sidebar-p t)
			     ,@(instantiate-plugins plugins)))))
      (mapcar (lambda (widget)
		(setf (widget-parent widget) home))
	      (composite-widgets home))
      home)))
			 
;; Defined in patient-home...

;;(defun make-user-map-widget ()
;;  (make-instance 'user-map-widget
;;		 :dataset (generate-user-map-dataset (mappable-users))))

;; Defined in patient-home...

;;(defun mappable-users ()
;;  (select-if (curry #'get-preference 'map-me-p) (all-users)))

;; Defined in patient-home...

;;(defun no-javascript (&rest args)
;;  (declare (ignore args))
;;  (with-html 
;;    (:noscript :class "no-javascript" :style ".no-javascript font-size: small; border: 1px black;" (str #!"Your browser has disabled Javascript.  Many, but not all, features of this site depend on Javascript.  You can use the 'Collect' and 'Discuss' sections but for now the 'Explore' section may not function for you.  You can find out about how to enable Javascript from ") (:a :href "http://www.google.com/support/bin/answer.py?answer=23852" "Google's help center."))))


