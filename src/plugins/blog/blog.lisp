(in-package :registry)

(registry-proclamations)

(define-plugin blog-widget ()
  :create 'make-blog-widget)

(defwidget blog-widget (widget)
  ((flash :accessor blog-widget-flash :initarg :flash
	  :initform (make-instance 'flash))
   (ui-mode :accessor blog-widget-ui-mode :initarg :ui-mode
	    :initform :overview
	    :documentation "One of (:overview :entry)")
   (title :accessor blog-widget-title :initarg :title :initform "Site Updates")
   (entries-per-page :accessor blog-widget-entries-per-page
		     :initarg :entries-per-page :initform 2)
   (pagination :accessor blog-widget-pagination :initarg :pagination
	       :initform nil)
   (entries :accessor blog-widget-entries :initarg :entries :initform nil)
   (current-entry :accessor blog-widget-current-entry :initarg :current-entry
		  :initform nil
		  :documentation "The blog entry being displayed when
                                  ui-mode is :entry")))

(defmethod initialize-instance :after ((w blog-widget) &rest initargs)
  (declare (ignore initargs))
  (with-slots (entries-per-page pagination flash) w
    (setf pagination (make-instance 'pagination
				    :items-per-page entries-per-page
				    :show-total-items-p nil
				    :on-error flash
				    :on-change (lambda (&rest args)
						 (declare (ignore args))
						 (mark-dirty w)))))
  (load-blog-entries w))

(defun make-blog-page (&rest args)
  (make-instance 'composite :widgets
		 (list (apply #'make-blog-widget args))))

(defun make-blog-widget (&rest args)
  (apply #'make-instance 'blog-widget args))

;;; For now, just assume that all blog-entry instances are for the
;;; "what's new on the site" mini-blog.  It would be straightforward
;;; to extend the blog widget so that it could be smarter about
;;; selecting which entries to load.
(defmethod load-blog-entries ((w blog-widget))
  (let* ((entries (get-instances-by-class 'blog-entry))
	 (n (length entries))
	 (pagination (blog-widget-pagination w)))
  (setf (blog-widget-entries w)
	(sort entries #'> :key #'blog-entry-date))
  (setf (pagination-total-items pagination) n
	(pagination-current-page pagination) 1)))

(defun render-edit-blog-entry-link (widget entry)
  (render-link (make-action
		#'(lambda (&rest args)
		    (declare (ignore args))
		    (with-flow widget
		      (yield
		       (make-quickform 'blog-entry-edit-view
				       :data entry
				       :satisfies
				       #'valid-blog-entry-p
				       :on-success
				       #'(lambda (w o)
					   (declare (ignore w o))
					   (load-blog-entries widget))
				       )))))
	       "edit this entry"))

(defun render-one-entry (entry)
  (with-html
    (htm
     (:h2 (str (slot-value-translation entry 'title)))
     (:p :class "blog-entry-author" "by: " (str (username
						 (blog-entry-author entry))))
     (:span :class "datetime"
	    (present-as-static 'datetime-presentation (blog-entry-date entry)))
     (:div :class "blog-entry-content" (str (slot-value-translation entry 'content))))))

(defun valid-blog-entry-p (widget data)
  "Just a little paranoia."
  (declare (ignore widget data))
  (if (has-permission-p (current-user) (get-permission 'admin))
    t
    (values nil "Only administrators may add blog entries.")))

(defun add-blog-entry (data)
  (with-slots (title content) data
    (let ((entry (make-instance 'blog-entry :title title :content content)))
      (record-event :new-blog-entry entry))))

(defun render-add-blog-entry-link (widget)
  (render-link (lambda (&rest args)
		 (declare (ignore args))
		 (do-widget widget 
		   (make-quickform 'blog-entry-add-view
				   :satisfies
				   #'valid-blog-entry-p
				   :on-success
				   #'(lambda (w o)
				       (declare (ignore w))
				       (add-blog-entry o)
				       (load-blog-entries widget))
				   )))
	       "add a blog entry"))

(defmethod render-widget-body ((w blog-widget) &rest args)
  (declare (ignore args))
  (with-html
    (:h1 (str (blog-widget-title w))))
  (render-widget (blog-widget-flash w))
  (multiple-value-bind (l h)
      (pagination-page-item-range (blog-widget-pagination w))
    (let ((items (subseq (blog-widget-entries w)  l h)))
      (dolist (i items)
	(with-html
	  (:div :class "blog-entry"
		(render-one-entry i)
		(when (has-permission-p (current-user) (get-permission 'admin))
		  (render-edit-blog-entry-link w i))))
	#+nil
	(render-object-view i 'blog-entry-brief-view))))
  (when (has-permission-p (current-user) (get-permission 'admin))
    (render-add-blog-entry-link w))
  (render-widget (blog-widget-pagination w)))

