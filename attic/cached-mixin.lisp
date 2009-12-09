(in-package :registry)

(registry-proclamations)

;; Caching

(defclass cached-body-mixin ()
  ((cached-body-string :accessor cached-body-string :initform nil)))

(defmethod render-widget-body :around ((widget cached-body-mixin) &rest args)
  "Caches and/or overrides output"
  (declare (ignore args))
  (when (or (widget-dirty-p widget) (null (cached-body-string widget)))
    (setf (cached-body-string widget) 
	  (with-output-to-string (stream)
	    (let ((*weblocks-output-stream* stream))
	      (declare (special *weblocks-output-stream*))
	      (call-next-method))))
    (with-html (str (cached-body-string widget)))))


