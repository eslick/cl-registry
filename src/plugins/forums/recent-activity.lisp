(in-package :registry)

(registry-proclamations)

(defun recently-active-topics (since)
  (let* ((topics nil))
    (flet ((push-if-recent (topic)
	     (when (> (topic-date-updated topic) since)
	       (push topic topics))))
      (map-class #'push-if-recent 'forum-topic))
    (nreverse topics)))

(defun recent-posts (since)
  (let* ((posts nil))
    (flet ((push-if-recent (post)
	     (when (> (post-datetime post) since)
	       (push post posts))))
      (map-class #'push-if-recent 'forum-post))
    (nreverse posts)))

(defun recent-posts-in-topic (topic since)
  (loop for p in (get-instances-by-value 'forum-post 'topic topic)
     when (> (post-datetime p) since)
       collect p))

(defclass recent-activity-settings ()
  ((days-ago :initarg :days-ago :accessor settings-days-ago
	     :initform 7)
   (show-forums :initarg :show-posts
	       :accessor settings-show-forums
	       :initform t)
   (show-surveys :initarg :show-surveys
		 :accessor settings-show-surveys
		 :initform t)
   (minimum-rating :accessor settings-minimum-rating :initarg :minimum-rating
                   :initform 2)))

(defwidget recent-activity ()
  ((dataform :accessor recent-activity-dataform
	     :initarg :dataform
	     :initform
	     (make-instance 'dataform
			    :data-view 'recent-activity-settings-data-view
			    :form-view 'recent-activity-settings-form-view))
   (settings :accessor recent-activity-settings
	     :initarg :settings
	     :initform (make-instance 'recent-activity-settings))))

(defmethod initialize-instance :after ((obj recent-activity) &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (let ((dataform (recent-activity-dataform obj)))
    (setf (dataform-data dataform) (recent-activity-settings obj)
	  (dataform-on-success dataform) #'(lambda (dataform)
					    (declare (ignore dataform))
					    (mark-dirty obj)))))

(defview recent-activity-settings-data-view (:type data)
  days-ago
  (show-forums :present-as (predicate))
  (show-surveys :present-as (predicate))
#|  (minimum-rating :present-as (stars))|#)

(defview recent-activity-settings-form-view (:type form :persistp nil)
  (days-ago :present-as (input :max-length 4)
	    :parse-as (integer :min 1))
  (show-forums :present-as (checkbox)
               :parse-as (predicate))
  (show-surveys :present-as (checkbox)
                :parse-as (predicate))
#|  (minimum-rating :present-as
		  (dropdown :choices '(1 2 3 4 5)
			    :label-key #'(lambda (n) 
					   (make-star-string n)))
                  :parse-as integer) |#)

(defmethod render-widget-body ((obj recent-activity) &rest args)
  (declare (ignore args))
  (let ((settings (recent-activity-settings obj)))
    (flet ((process-form (&key days-ago rating show-forums show-surveys &allow-other-keys)
	     (let (n)
	       (setq n (ignore-errors (parse-integer days-ago)))
	       (when n 
		 (setf (settings-days-ago settings) n)))
	     (setf (settings-minimum-rating settings)
		   (ignore-errors (parse-integer rating)))
	     (setf (settings-show-forums settings) show-forums)
	     (setf (settings-show-surveys settings) show-surveys)

	     (mark-dirty obj)))
      (with-html
	(:div :id "recent-activity"
	      (with-html-form (:post #'process-form)
		(:label
		 (render-text-input "days-ago" (settings-days-ago settings))
		 "days back")
		(:label
		 "Minimum rating: "
		 (render-dropdown "rating" `((,(make-star-string 1) . 1)
					     (,(make-star-string 2) . 2)
					     (,(make-star-string 3) . 3)
					     (,(make-star-string 4) . 4)
					     (,(make-star-string 5) . 5))
				  :selected-value
				  (format nil "~d" (settings-minimum-rating settings))
				  :autosubmitp t
				  :welcome-name "rating"))
		(:label
		 (render-checkbox "show-forums" (settings-show-forums settings))
		 "Show forums")
		(:label
		 (render-checkbox "show-surveys" (settings-show-surveys settings))
		 "Show surveys")
		(:div :class "buttons"
		      (render-translated-button "submit")))
	      (:hr)
	      (when (settings-show-forums settings)
		(let* ((min-rating (settings-minimum-rating settings))
		       (since (- (get-universal-time)
				 (* (settings-days-ago settings) 86400)))
		       (topics (recently-active-topics since)))
                  (declare (ignore min-rating))
;; 		  (when min-rating
;; 		    (setq topics (remove-if #'(lambda (topic)
;; 						(let ((r (rating-value (topic-rating topic))))
;; 						  (or (null r)
;; 						      (< r min-rating))))
;; 					    topics)))
		  (when topics
		    (with-html
		      (:div :class "recent-topics"
			    (:table
			     (dolist (topic topics)
			       (let* ((n (length (recent-posts-in-topic topic since))))
				 (with-html
				   (:tr
				    (:td
				     (:a :href (make-webapp-uri
						(format nil "/dashboard/forums/topic/~d"
							(topic-number topic)))
					 (str (topic-subject topic))))
				    (:td (str (format nil "~d recent messages" n))))))))))))))))))
