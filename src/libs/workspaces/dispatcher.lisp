(in-package :registry)

(defwidget dispatcher (selector)
  ((cached :accessor disp-cached :initform nil)))

(defmethod get-widget-for-tokens :around ((disp dispatcher) uri-tokens)
  (declare (ignore uri-tokens))
  (let ((result (call-next-method)))
    (setf (disp-cached disp) result)
    result))

(defmethod render-widget-body ((disp dispatcher) &rest args)
  (declare (ignore args))
  nil)

(defmethod render-widget-children ((disp dispatcher) &rest args)
  (when (disp-cached disp)
    (apply #'render-widget (disp-cached disp) args)))