(in-package :registry)

;;
;; Widget
;;

(defwidget visualization-widget ()
  ((objects :accessor viz-objects :initarg :objects)))

(defmethod dependencies append ((obj visualization-widget))
  (list (make-local-dependency :script "discovery/viz")
	(make-instance 'script-dependency 
		       :url "http://visapi-gadgets.googlecode.com/svn/trunk/wordcloud/wc.js")
	(make-instance 'stylesheet-dependency
		       :url "http://visapi-gadgets.googlecode.com/svn/trunk/wordcloud/wc.css")))

(defun make-viz-widget ()
  (make-instance 'visualization-widget))


;;
;; Render the body
;;

(defmethod render-widget-body ((widget visualization-widget) &rest args)
  (declare (ignore args))
  (with-html
    (:div :class "viz-test-wrapper" :style "margin-top: 100px"
	  (render-link (f* (set-objects widget)) "Reset Objects")
	  (dolist (viz (viz-objects widget))
	    (htm (:p (render-visualization viz)))
	    (htm (:hr))))))


;; =============================================
;; Default test data
;;

(defparameter *simple-dataset*
  '((("string" "Task") "Work" "Eat" "Commute" "Watch TV" "Sleep" "Other")
    (("number" "Hours per Day") 10 3 2 2 4 3)))

(defparameter *word-dataset*
  '((("string" "text") "This is a test" "I am broke" "I will test to see" "If I run, then you hide" "If you can't see, I will test you")))

(defparameter *imap-dataset*
  '((("string" "" "Country") 
     "CN" "IN" "US" "ID" "BR")
    (("number" "Population (mil)" "a")
     1324 1133 304 232 187)
    (("number" "Area (km2)" "b")
     9640821 3287263 9629091 1904569 8514877)))

(defmethod initialize-instance :after ((widget visualization-widget) &rest initargs)
  (declare (ignore initargs))
  (set-objects widget))

(defun set-objects (widget)
  (setf (viz-objects widget)
	(list
	 (let ((question (get-question 6058)))
	   (make-instance 'bar-chart-visualization 
			  :object question
			  :query-fn 'distribution-query
			  :parameters `((width . 400)
					(height . 400)
;;					(is3-D . true)
;;					(reverse-axis . "false")
					(legend . bottom)
					(title . ,(question-prompt question))
					(background-color . "F0EEF4")
					(legend-background-color . "F0EEF4"))))
	 (let ((question (get-question 6002)))
	   (make-instance 'pie-chart-visualization 
			  :object question
			  :query-fn 'distribution-query
			  :parameters `((width . 400)
					(height . 400)
;;					(is3-D . true)
;;					(reverse-axis . "false")
					(legend . bottom)
					(title . ,(question-prompt question))
					(background-color . "F0EEF4")
					(legend-background-color . "F0EEF4"))))
	 (let ((question (get-question 5912)))
	   (make-instance 'wordcloud-visualization
			  :dataset (make-word-cloud-dataset
				    (get-answer-values* question))
			  :parameters `((width . 500)
					(height . 300)
					(title . ,(question-prompt question))))))))

#|
	(list
	 (make-instance 'pie-chart-visualization
			:dataset *simple-dataset*
			:parameters '((width . 400)
				      (height . 240)
				      (is3D . true)
				      (title . "Bar Viz")))
	 (make-instance 'bar-chart-visualization
			:dataset *simple-dataset*
			:parameters '((width . 400)
				      (height . 240)
				      (is3D . true)
				      (title . "Bar Viz")))
	 (make-instance 'column-chart-visualization
			:dataset *simple-dataset*
			:parameters '((width . 400)
				      (height . 240)
				      (is3D . true)
				      (title . "Column Viz")))
	 (make-instance 'wordcloud-visualization
			:dataset *word-dataset*
			:parameters nil)
	 (make-instance 'area-chart-visualization
			:dataset *simple-dataset*
			:parameters '((width . 400)
				      (height . 240)
				      (is3D . true)
				      (title . "Column Viz")))
	 (make-instance 'line-chart-visualization
			:dataset *simple-dataset*
			:parameters '((width . 400)
				      (height . 240)
				      (is3D . true)
				      (title . "Column Viz")))
	 (make-instance 'intensitymap-visualization
			:dataset *imap-dataset*
			:parameters '((region . asia)
				      (width . 800))))))
|#
