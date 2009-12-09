(in-package :registry)

(defparameter *tinymce-default-height* 300)

(defun apply-tinymce (target &key (height *tinymce-default-height*))
  "Outputs JS that applies the TinyMCE editor to the TARGET textarea.
  FIXME: probably belongs to JS utility library file."
  (with-javascript
    (ps:ps* `(progn
               (setf (slot-value |:tinyMCE| 'id-counter) 0)
               (setf (slot-value |:tinyMCE| 'settings)
                     (create :height ,height
                             :language "en"
                             :content_css "/pub/stylesheets/tinymce.css"

                             :theme "advanced"
                             :theme_advanced_toolbar_location "top"
                             :theme_advanced_toolbar_align "left"
                             :theme_advanced_layout_manager "SimpleLayout"

                             ;:plugins "table,style,searchreplace,fullscreen,paste"
                             ))
               (.exec-command |:tinyMCE| "mceAddControl" t ,target)))))

(defclass tinymce-presentation (textarea-presentation)
  ())

(defmethod render-view-field-value (value (presentation tinymce-presentation)
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
      (apply-tinymce id))))