(in-package :registry)

;; 
;; Rounded box styling
;;


(defun call-in-liquid-context (fn)
  (with-html
    (:div :class "liquid-round"
	  (:div :class "top" (:span))
	  (:div :class "center-content"
		(funcall fn))
	  (:div :class "bottom" (:span)))))


(defwidget rounded-composite (composite) 
  ())

(defmethod render-widget-body ((box rounded-composite) &rest args)
  (declare (ignore args))
  (call-in-liquid-context 
   (lambda () (call-next-method))))

(defun make-liquid-box (&rest widgets)
  (apply #'make-instance 'rounded-composite
	 :widgets widgets))

