(in-package :registry)

;;
;; Simple recent activity widget
;;

(define-plugin recent-topics-widget ()
  "Adds a recent forum topics widget"
  :create 'make-recent-topics-widget)

(defwidget recent-topics-widget ()
  ())

(defun make-recent-topics-widget ()
  (make-instance 'recent-topics-widget))

;; This doesn't scale
(defun recent-forum-topics (&key (count 5) (center (current-center)))
  (if *forum-category-per-center-p*
      (let ((topics nil))
        (when center
          (dolist (category (get-instances-by-value 'forum-category 'center center))
            (dolist (topic (get-instances-by-value 'forum-topic 'category category))
              (push topic topics)))
          (subseq (sort topics #'> :key #'topic-date-created)
                  0 (min (length topics) count))))
      (find-persistent-objects *registry-main* 'forum-topic
                               :order-by '(date-updated . :desc)
                               :range '(cons 0 count))))

(defmethod render-widget-body ((widget recent-topics-widget) &rest args)
  (declare (ignore args))
  (let ((popular-forum-topics (recent-forum-topics)))
   (with-html
     (:h1 (str #!"Recent Forum Activity"))
     (:ul
      (dolist (topic popular-forum-topics)
	(with-html
	  (:li (:a :href (format nil "/dashboard/discuss/topic/~d" (topic-number topic))
		   (str (topic-subject topic))))))))))
