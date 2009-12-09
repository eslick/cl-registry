(in-package :registry)

(defwidget featured-survey-widget ()
  ((features :accessor features :initarg :features
	     :documentation "A list of questions to present, selected randomly")
   (current :accessor current-feature :initarg :current)
   (visualization :accessor current-visualization :initarg visualization)))

(defun make-featured-survey-widget (questions)
  (let ((widget (make-instance 'featured-survey-widget :features questions
			       :current (first questions))))
    (update-visualization widget)
    widget))

(defmethod dependencies append ((view featured-survey-widget))
  (list (make-local-dependency :stylesheet "explorer")
	(make-local-dependency :stylesheet "wc")
	(make-local-dependency :script "discovery/viz")))

(defmethod render-widget-body ((widget featured-survey-widget) &rest args)
  (declare (ignore args))
  (with-html
    (:div :class "featured-survey-header"
	  (:h1 "Featured Survey Question Results"))
    (:p (str (question-prompt (current-feature widget))))
    (:div :class "featured-survey-viz" :style "min-height: 260px;"
	  (render-visualization (current-visualization widget)))
    (:div :class "featured-survey-footer"
	  (render-link (f* (next-feature widget)) "Next Feature >>"))
    (:p)))
	  

(defun next-feature (widget)
  (with-slots (current features) widget
    (if (or (null current)
	    (eq current (last1 features)))
	(setf current (first features))
	(setf current (cadr (member current features))))
    (update-visualization widget)))

(defun update-visualization (widget)
  (let ((viz (make-result-summary-visualization (current-feature widget) 
						:constraints nil)))
    (maybe-set-parameter viz 'title nil)
    (maybe-set-parameter viz 'width 350)
    (maybe-set-parameter viz 'height 250)
    (setf (current-visualization widget) viz)
    viz))


