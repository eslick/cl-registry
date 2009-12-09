(in-package :registry)

;;; Date and time presentation for weblocks views.  Note that this is
;;; distinct from the other presentations defined in
;;; translation/src/libs/presentations.

;;; The wl- prefix is nasty, but our local presentation system has
;;; usurped the -presentation suffix, so we have to do something.

(defun format-datetime-display (datetime &key (show-time-p t) (show-date-p t))
  (with-string-stream (stream)
    (cl-l10n:print-time datetime :show-date show-date-p
			:show-time show-time-p :stream stream)))

(defclass wl-datetime-presentation (text-presentation input-presentation)
  ())

(defmethod print-view-field-value (datetime (presentation wl-datetime-presentation)
				   field view widget obj &rest args)
  (declare (ignore args obj widget field view))
  (format-datetime-display datetime))

(defclass wl-date-presentation (wl-datetime-presentation)
  ())

(defmethod print-view-field-value (date (presentation wl-date-presentation)
				   field view widget obj &rest args)
  (declare (ignore args obj widget field view))
  (format-datetime-display date :show-time-p nil))

;; Datetime Renderers

(defmethod render-view-field-value (value (presentation wl-datetime-presentation)
				    field view widget obj &rest args)
  (declare (ignore args obj widget view field))
  (with-html
    (str (format-datetime-display value :show-time-p t))))

(defmethod render-view-field-value (value (presentation wl-datetime-presentation)
				    (field form-view-field) (view form-view)
				    widget obj &rest args)
  (declare (ignore args obj widget))
  (render-text-input (view-field-slot-name field)
		     (format-datetime-display value :show-time-p t)
		     :maxlength 50))

;; Date renderers

(defmethod render-view-field-value (value (presentation wl-date-presentation)
				    field view widget obj &rest args)
  (declare (ignore args obj widget view field))
  (with-html
    (str (format-datetime-display value :show-time-p nil))))

(defmethod render-view-field-value (value (presentation wl-date-presentation)
				    (field form-view-field) (view form-view)
				    widget obj &rest args)
  (declare (ignore args obj widget))
  (render-text-input (view-field-slot-name field)
		     (format-datetime-display value :show-time-p nil)
		     :maxlength 50))

;; Parser

(defclass wl-datetime-parser (parser)
  ()
  (:default-initargs :error-message "a valid date and time (e.g. 2008-01-01 10:11)")
  (:documentation "A locale sensitive parser for date+time representations"))

(defmethod parse-view-field-value ((parser wl-datetime-parser) value obj
				   (view form-view) (field form-view-field) 
				   &rest args)
  (declare (ignore args obj) (optimize safety))
  (parse-datetime value))

(defun parse-datetime (value)
  (handler-case
      (let* ((presentp (text-input-present-p value))
	     (dt-value (when presentp (cl-l10n:parse-time value))))
	(values t presentp dt-value))
    (error (err) (print err) nil)))

