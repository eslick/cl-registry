
(in-package :registry)

(registry-proclamations)

;;
;; Export as site application
;;

(define-plugin patient-home (site-app)
  "The create function accepts a list of plugins"
  :tab-name 'home
  :create 'make-patient-home-page)

;;
;; Patient home page
;;

(defwidget patient-home (composite)
  ((workspace :accessor home-page-workspace :initarg :workspace 
	      :initform nil)
   (map :accessor home-page-map :initarg :map)
   (laura-welcomed-p :accessor home-page-welcome-p :initform t)
   (laura-welcome-text :accessor laura-welcome-text :initarg :welcome-text)))

(defmethod render-widget-children ((home patient-home) &rest args)
  (declare (ignore args))
  nil)

(defmethod render-widget-body ((widget patient-home) &rest args)
  (declare (ignore args))
  (with-html
    (:div :class "home-main-pane"
	  (render-widget (first (composite-widgets widget))))
    (:div :class "home-sidebar"
	  (:div :class "top"
		(mapcar #'render-widget (rest (composite-widgets widget))))
	  (:div :class "bottom"
		(str "&nbsp;")))))
;;   (when (home-page-welcome-p widget)
;;    (mapc #'laura-speak 
;;	  (mapcar #'find-translation (mklist (laura-welcome-text widget))))
;;    (setf (home-page-welcome-p widget) nil)))

(defparameter *featured-survey-question-ids*
  '(5858 5854 5856 6074 6076 6123))

(defun make-patient-home-page (&key plugins &allow-other-keys)
  (declare (ignore plugins))
  (let ((home (make-instance 'patient-home 
	       :widgets
	       `(,(make-instance 'composite
				 :widgets
				 (list (make-widget 'no-javascript)
				       (make-article-widget "dashboard-home"
							    :sidebar-p t)))
		  ,(make-user-controls)
		  ,(make-user-map-widget)
		  ,(make-recent-topics-widget)
;;		  ,(make-featured-survey-widget
;;		    (mapcar #'get-question *featured-survey-question-ids*))
		  ,(make-instance 'blog-widget
				  :entries-per-page 1
				  :title "Breaking News")
		  ))))
    (mapcar (lambda (widget)
	      (setf (widget-parent widget) home))
	    (composite-widgets home))
    (setf (home-page-map home) (third (composite-widgets home)))
    home))
			 
(defun make-user-map-widget ()
  (make-instance 'user-map-widget
		 :dataset (generate-user-map-dataset (mappable-users))))

(defun mappable-users ()
  (select-if (curry #'get-preference 'map-me-p) (all-users)))

(defun no-javascript (&rest args)
  (declare (ignore args))
  (with-html 
    (:noscript :class "no-javascript" :style ".no-javascript font-size: small; border: 1px black;" (str #!"Your browser has disabled Javascript.  Many, but not all, features of this site depend on Javascript.  You can use the 'Collect' and 'Discuss' sections but for now the 'Explore' section may not function for you.  You can find out about how to enable Javascript from ") (:a :href "http://www.google.com/support/bin/answer.py?answer=23852" "Google's help center."))))


