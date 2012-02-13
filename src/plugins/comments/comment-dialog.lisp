(in-package :registry)

;; Survey comments

(defun/cc do-question-comment-dialog (controller object &optional (type :question))
  (do-dialog #!"View and Add Comments"
    (make-instance 'comment-widget :ctrl controller :type type :question object)
    :css-class "survey-view-comment-widget"))
    

(defwidget comment-widget ()
  ((controller :initarg :ctrl :accessor comment-widget-controller)
   (object :initarg :question :accessor comment-widget-object)
   (type :initarg :type :accessor comment-widget-object-type)
   (form :accessor comment-widget-form)))

(defmethod initialize-instance :after ((widget comment-widget) &rest initargs)
  (declare (ignore initargs))
  (setf (comment-widget-form widget)
	(make-quickform 'comment-add-view
			:answerp nil
			:data-class-name 'comment
			:on-cancel
			(lambda (qform)
			  (declare (ignore qform))
			  (answer widget))
			:on-success
			(lambda (qform temp)
			  (declare (ignore qform))
			  (mark-dirty (comment-widget-controller widget))
			  (answer widget 
				  (add-comment (comment-widget-object widget)
					       (comment-content temp)))))))
					       


(defmethod render-widget-body ((widget comment-widget) &rest args)
  "We should abstract by-type rendering out from here and survey-ctrl since
   we'll be using it elsewhere too (survey-editor, explorer)"
  (declare (ignore args))
  (with-slots (type object form) widget
    (call-in-liquid-context
     (ecase type
       (:question (lambda () (render-question-comment-view object)))
       (:group (lambda () (render-group-comment-view object)))))
    (render-widget form)
    (render-existing-comments widget)))

(defun render-comment-header ()
  (with-html
    (:hr)
    (:p (:b "Comments are intended for you to ask the author of the survey clarifying questions or to add information relevant to your answer that isn't captured elsewhere in the survey.  These comments are visible to other users."))
    (:hr)))

(defun render-question-comment-view (question)
  (with-html
    (render-comment-header)
    (:div :class "question"
	  (:div :class "question-prompt"
                (str (slot-value-translation question 'prompt)))
	  (:div :class "question-input"
		(render-presentation (make-presentation question 1)))
	  (:div :class "question-separator-1" "&nbsp")
	  (when (eq (question-data-type question) :measurement)
	    (htm (:div :class "question-hint" :style "font-size: small; text-align: right;"
		       (str #!"We will support English units soon.  For now you can convert units at") 
		       (:a :href "http://www.worldwidemetric.com/metcal.htm"
			   (str #!"this address.")))))
	  (awhen (slot-value-translation question 'question-help)
	    (when (> (length it) 2)
	      (htm (:div :class "question-help" 
			 (str it))))))))

(defun render-group-comment-view (group)
  (with-html
    (render-comment-header)
    (:div :class "group"
	  (:div :class "group-name"
		(str (group-name group))))))

(defun render-comments (comments)
  (let* ((sorted-comments (sort comments #'> :key #'comment-date)))
    (with-html
      (:div :class "existing-comments"
        (if sorted-comments
	  (mapc #'render-comment sorted-comments)
	  (str #!"No comments"))))))

(defun render-existing-comments (widget)
  (render-comments 
   (get-comments (comment-widget-object widget))))

(defun render-comment (comment)
  (with-html
    (:div :class "comment-view"
	  (awhen (comment-author comment)
	    (htm (:p :class "comment-author" (str #!"by: ")
		     (str (username it)))))
	  (:span :class "comment-datetime"
		 (present-as-static 'datetime-presentation (comment-date comment)))
	  (:p :class "comment-content" (str (comment-content comment))))))
    
    