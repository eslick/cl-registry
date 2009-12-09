(in-package :registry)

(registry-proclamations)

;;; View for forum posts
(defclass post-view (data-view)
  ())

;;; View fields in forum posts
(defclass post-view-field (data-view-field)
  ())

;;; Make scaffolding system happy
(defclass post-scaffold (data-scaffold)
  ())

;;; Implement rendering protocol
(defmethod with-view-header ((view post-view) obj widget body-fn &rest args &key
			     (fields-prefix-fn (view-fields-default-prefix-fn view))
			     (fields-suffix-fn (view-fields-default-suffix-fn view))
			     &allow-other-keys)
  (declare (ignore widget))
  (let* ((fields (view-fields view))
	 (content-field (find "content" fields :test #'string-equal
			      :key #'view-field-slot-name)))
    (declare (ignore content-field))
    (with-html
      (:div :class "forum-post"
	    (with-extra-tags
	      (htm
	       (:h1 (str (post-title obj)))
	       (:h2 :class "post-user"
		    (str (and (post-owner obj) (username (post-owner obj)))))
	       (safe-apply fields-prefix-fn view obj args)
	       (apply body-fn view obj args)
	       (safe-apply fields-suffix-fn view obj args)))))))

(defmethod render-view-field ((field post-view-field) (view post-view)
			      widget presentation value obj
			      &rest args)
  (if (typep presentation 'star-rating-presentation)
    (call-next-method)
    (apply #'render-view-field-value
	   value presentation field view widget obj args)))

(defmethod render-view-field-value (value (presentation paragraph-presentation)
				    field (view post-view) widget obj &rest args
				    &key highlight &allow-other-keys)
  (declare (ignore highlight args obj widget field value))
  (call-next-method)
  ;; this code is just copy-and-pasted, the only
  ;; difference being the paragraph class.
  ;; probably want to use some simple markup language here,
  ;; rather than just putting in <br /> tags.
  #+nil
  (if (null value)
      (call-next-method)
      (let* ((item (apply #'print-view-field-value value presentation field view widget obj args))
	     (lit-item (if highlight
			   (highlight-regex-matches item highlight)
			   (hunchentoot:escape-for-html item))))
	(with-html
	  (:p :class "post-content"
	      (str (apply #'concatenate 'string
			  (intersperse (metatilities:tokenize-string lit-item
							:delimiter #\Newline
							:include-empties? t)
				       "<br />"))))))))

(defmethod render-view-field-value (value (presentation datetime-presentation)
				    field (view post-view) widget obj &rest args
				    &key highlight &allow-other-keys)
  (declare (ignore highlight args obj widget field value))
  (with-html
    (:p :class "datetime"
	(call-next-method))))
