(in-package :registry)

;;
;; Simple recent activity widget
;;

(defwidget recent-topics-widget ()
  ())

(defun make-recent-topics-widget ()
  (make-instance 'recent-topics-widget))

(defmethod render-widget-body ((widget recent-topics-widget) &rest args)
  (declare (ignore args))
  (let ((popular-forum-topics
	 (find-persistent-objects *registry-main* 'forum-topic
				  :order-by '(date-updated . :desc)
				  ;; :range '(0 . 5) ; commented because BUG if list isn't as big as range.
				  )))
   (with-html
     (:h1 (str #!"Recent Forum Activity"))
     (:ul
      (dolist (topic popular-forum-topics)
	(with-html
	  (:li (:a :href (format nil "/dashboard/discuss/topic/~d" (topic-number topic))
		   (str (topic-subject topic))))))))))
