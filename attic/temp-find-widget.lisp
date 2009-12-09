(in-package :weblocks)


;;; the guts of what we want to test
(defgeneric find-widget (id &rest args))

(defmethod find-widget ((id string) &key (root (root-composite)) (test #'equal)
                                    (include-hidden-p t) (token-accum nil))
  (if (funcall test (dom-id root) id)
    root
    (loop for w in (etypecase root
                     (composite (composite-widgets root))
                     (navigation (union
                                   (list (cdr (dispatcher-cache root)))
                                   (when include-hidden-p
                                     (mapcar #'cdr (selector-mixin-panes root)))))
                     (selector-mixin (mapcar #'cdr (selector-mixin-panes root)))
                     (dispatcher (awhen (cdr (dispatcher-cache root))
                                     (list it)))
		     (t nil))
          do
	  (progn ;;(format t "In find widget; searching ~A~%" w)
           (multiple-value-bind (found tokens) (find-widget id :root w)
             (setf token-accum (append tokens token-accum))
             (when found
;;	       (format t "   found: ~A~%" found)
               (when (typep root 'selector-mixin)
                 (push (car (find w (selector-mixin-panes root) :key #'cdr))
                       token-accum))
               (return-from find-widget (values found token-accum)))))
          finally (return nil))))

(defmethod find-widget ((id symbol) &rest args)
  (apply #'find-widget (symbol-name id) :test #'equalp args))

(defmethod find-widget ((widget widget) &rest args)
  (apply #'find-widget (dom-id widget) args))

(defparameter *fail-on-missing-path* t)

;;(defmethod render-widget :before ((widget dispatcher) &rest args)
;;  (declare (ignore args))
;;  (and *fail-on-missing-path* (ajax-request-p) (not (find-widget widget))
;;      (error "Can't update dirty widget ~A because it's ephemeral!" widget)))


(defgeneric get-widget-uri-tokens (widget &key state)
  (:documentation "Specialize for your own widgets to convert state to an URL")
  (:method ((widget widget) &rest args)
    (declare (ignore args))
    (nth-value 1 (find-widget widget))))

(defun get-widget-uri-tokens-by-parent (widget &key (root (root-composite)))
  (when (not (eq widget root))
    (let ((parent (widget-parent widget)))
      (typecase parent
	(dispatcher (append (get-widget-uri-tokens-by-parent parent) (awhen (car (dispatcher-cache parent))
							      (if (listp it)
								  it
								  (list it)))))
	(t (get-widget-uri-tokens-by-parent parent))))))

