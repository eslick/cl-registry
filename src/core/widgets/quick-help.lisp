(in-package :registry)

(defwidget quick-help ()
  ((dialog-p :initarg :dialog-p)
   (page :initarg :page)
   (link :initarg :link)
   (link-title :initarg :link-title))
  (:documentation "Quick and dirty help widget that links to a dialog or page"))

(defmethod render-widget-body ((help quick-help) &rest args)
  (declare (ignore args))
  (with-slots (dialog-p page awidget link link-title) help
    (labels ((dialog-action (&rest args)
	       (declare (ignore args))
	       (do-dialog ""
		 (make-article-widget page 
				      :render-title-p t
				      :render-answer-p t))))
      (with-html
	(if page
	    (htm (render-link #'dialog-action (concatenate 'string link-title (with-html-to-string (:img :src "/pub/images/help.png"))) :class "button"))
	    (htm (:a :class "button" :href (str link-title) (:img :src "/lspubs/images/help.png"))))))))