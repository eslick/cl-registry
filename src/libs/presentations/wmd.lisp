(in-package :registry)

(defparameter *wmd-default-height* 300)

(defun apply-wmd (target)
  "Outputs JS that applies the WMD editor to the TARGET textarea.
  FIXME: probably belongs to JS utility library file."
  (let ((preview-id (gen-id "preview")))
    (with-html
      (:div :class "wmd-preview" :id preview-id))
    (with-javascript
        "Attacklab.go($('~a'), $('~a'));"
      target preview-id)))

(defclass wmd-presentation (textarea-presentation)
  ())

(defmethod render-view-field-value (value (presentation wmd-presentation)
                                    (field form-view-field) (view form-view) widget obj
                                    &rest args &key intermediate-values &allow-other-keys)
  (multiple-value-bind (intermediate-value intermediate-value-p)
      (form-field-intermediate-value field intermediate-values)
    (let ((id (gen-id)))
      (render-textarea (attributize-name (view-field-slot-name field))
                       (if intermediate-value-p
                         intermediate-value
                         (apply #'print-view-field-value value presentation
                                field view widget obj args))
                       (textarea-presentation-rows presentation)
                       (textarea-presentation-cols presentation)
                       :id id)
      (apply-wmd id))))