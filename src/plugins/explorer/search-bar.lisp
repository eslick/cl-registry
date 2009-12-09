
(in-package :registry)

(registry-proclamations)

(defwidget search-bar ()
  ((on-search :accessor search-bar-on-search :initarg :on-search :initform nil
	     :documentation "The function to handle the search; called from
                             the action handler for the search bar w/ search bar.
                             Takes an argument ")
   (title :accessor search-bar-title :initarg :title)
   (value :accessor search-bar-value :initarg :search-value :initform "")))
  

(defmethod render-widget-body ((bar search-bar) &rest args)
  (declare (ignore args))
  (with-html
    (with-html-form (:post (lambda (&rest args &key query &allow-other-keys)
			     (declare (ignore args))
			     (setf (search-bar-value bar) query)
			     (safe-funcall (search-bar-on-search bar) bar query)))
      (:p (str (find-translation (search-bar-title bar)))
	  (render-text-input "query" 
			     (search-bar-value bar)
			     :class "search-bar-text" :maxlength 80)
	  (render-button "Go" :class "search-bar-go" :value #!"Go")))))

(defun make-search-bar (title on-search &optional name default)
  (let ((instance (make-instance 'search-bar 
				 :title title
				 :on-search on-search 
				 :search-value default
				 :peer-name name)))
    instance))
   