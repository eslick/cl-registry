(in-package :registry)

;;;; * web presentations

;;;; nominally we'd use a protocl based object system here, but CLOS
;;;; is fine.

(defvar *web-field-counter* 0)

(defun genweb-field-name ()
  (format nil "web-field-~D" (incf *web-field-counter*)))

(defclass web-field-presentation ()
  ((client-value :accessor client-value :initform "")
   (query-name :initarg :query-name
               :initform (genweb-field-name)
               :accessor query-name)
   (css-class :initarg :css-class :initform nil :reader css-class)
   (css-style :initarg :css-style :initform nil :reader css-style)
   (dom-id    :initarg :dom-id :initform (genweb-field-name) :accessor dom-id)
   (on-change-validation :initarg :on-change-validation
                         :reader on-change-validation
                         :initform nil)
   (required :accessor required :initarg :required :initform nil)
   (validators :accessor validators :initarg :validators :initform '())
   (prompt :accessor prompt :initarg :prompt :initform nil)
   (warning-message :accessor warning-message :initform nil
                    :documentation "If NIL this field's value is
                    currently valid, otherwise a string explaining why
                    the field's current value isn't valid.")
   (metadata :accessor metadata :initarg :metadata :initform nil)))

(defmethod print-object ((field web-field-presentation) stream)
  (print-unreadable-object (field stream :type t :identity t)
    (format stream "query-name: ~S client-value: ~S" (query-name field) (client-value field))))

(defmethod shared-initialize :after ((presentation web-field-presentation) slot-names
                                     &rest args &key (lisp-value nil lisp-value-p) (client-value nil client-value-p))
  (declare (ignore args client-value slot-names))
  (when lisp-value-p
    (when client-value-p
      (error "Can not initialize a presentation object with both a lisp-value and client-value."))
    (setf (lisp-value presentation) lisp-value))
  (when (slot-boundp presentation 'query-name)
    (setf (query-name presentation)
          (etypecase (query-name presentation)
            (string
               ;; weblocks wants to down case strings in attributes. i
               ;; don't know why.
               (string-downcase (query-name presentation)))
            (symbol
               (string-downcase (symbol-name (query-name presentation))))))))

(defgeneric lisp-value (presentation))

(defmacro define-lisp-value-getter (presentation-class-name slots &body body)
  `(defmethod lisp-value ((,presentation-class-name ,presentation-class-name))
     (with-accessors ,(mapcar (lambda (slot-name)
                                (list slot-name slot-name))
                              slots)
         ,presentation-class-name
       ,@body)))

(defgeneric (setf lisp-value) (new-value presentation))

(defmacro define-lisp-value-setter (presentation-class-name (new-value &rest slots) &body body)
  `(defmethod (setf lisp-value) (,new-value (,presentation-class-name ,presentation-class-name))
     (with-accessors ,(mapcar (lambda (slot-name)
                                (list slot-name slot-name))
                              slots)
         ,presentation-class-name
       (setf (client-value ,presentation-class-name) (progn ,@body))
       (setf (warning-message ,presentation-class-name)
             (multiple-value-bind (validp error-message)
                 (validp ,presentation-class-name)
               (if validp nil error-message)))
       ,new-value)))

(defgeneric render-presentation (presentation)
  (:method ((presentation web-field-presentation))
    (with-html
      (if (or (css-class presentation) (dom-id presentation))
          (htm (:span :id (dom-id presentation)
                      :class (css-class presentation)
                 (str (client-value presentation))))
          (str (client-value presentation))))))

(defun present-as-static (type value &rest presentation-init-args)
  "This could use a better name, but present-as used in the blog widget
   displayed an input, we probably want a present-as for non forms and
   present-as-editable for forms"
  (render-presentation (apply #'make-instance
			      type :lisp-value value
			      presentation-init-args)))
  

(defgeneric render-prompt (presentation)
  (:method ((presentation web-field-presentation))
    (with-html
      (:label :for (query-name presentation) 
	      (str (prompt presentation))))))

(defgeneric render-presentation-editable (presentation))

(defun present-as (type value &rest presentation-init-args)
  (render-presentation-editable (apply #'make-instance
                                       type :lisp-value value
                                       presentation-init-args)))

(defclass web-field-validator ()
  ())

(defgeneric validp (presentation)
  (:documentation "If PRESENTATION is valid according to its
  validators returns T, otherwise returns three values: NIL, an error
  message describing the failure and the web-field-validator object
  which failed.")
  (:method ((presentation web-field-presentation))
    (when (and (null (lisp-value presentation))
	       (not (required presentation)))
      (return-from validp t))
    (dolist (validator (validators presentation))
      (multiple-value-bind (validp error-message)
          (lisp-validate validator (lisp-value presentation))
        (when (not validp)
          (return-from validp (values nil error-message validator)))))
    t))

(define-condition invalid-presentation-value (error)
  ((message :initarg :message :reader message
            :documentation "A string, currently not localized,
            describing the validation failure.")
   (failed-validator :initarg :failed-validator :reader failed-validator
                     :documentation "The web-field-validator object
                     which failed."))
  (:documentation "Condition signaled by ENSURE-VALID when a
  presentation fails its validation."))

(defgeneric ensure-valid (presentation)
  (:documentation "If PRESENTATION is not valid signals an error, otherwise returns T.")
  (:method ((presentation web-field-presentation))
    (multiple-value-bind (validp error-message failed-validator)
        (validp presentation)
      (if validp
          t
          (error 'invalid-presentation-value
                 :message error-message
                 :failed-validator failed-validator)))))

(defgeneric lisp-validate (validator lisp-value)
  (:documentation "If LISP-VALUE passes VALIDATOR's validation returns
  T. Otherwise returns 2 values: NIL and an error message.")
  (:method ((validator web-field-validator) (lisp-value t))
    t))

(defgeneric client-validate (validator client-value)
  (:documentation "Test the string client-value against
  VALIDATOR. Returns two values like lisp-validate.")
  (:method ((validator web-field-validator) (client-value string))
    t))

(defun fail-validation (message &rest message-args)
  "Utility fuction for signaling a failed validatien. Returns two
values: NIL and the string resulting from message-args applied to
message (via format)."
  (values nil (if message-args
                  (apply #'format nil message message-args)
                  message)))

(defclass non-nil-validator (web-field-validator)
  ((error-message :reader error-message :initarg :error-message)))

(defmethod lisp-validate ((validator non-nil-validator) (lisp-value t))
  (if lisp-value
      t
      (fail-validation (error-message validator))))

(defmethod update-presentation ((presentation web-field-presentation) args)
  "Update a presentation's client-state from the appropriate arglist"
  (let ((provided-value (getf-all args (as-argument-keyword (query-name presentation)))))
    (when (and (or (null provided-value)
		   (equal provided-value ""))
	       (not (required presentation)))
      (return-from update-presentation (values t "")))
    (block test-validity
      (dolist (validator (validators presentation))
        (multiple-value-bind (validp error-message)
            (client-validate validator provided-value)
          (when (not validp)
	    (setf (warning-message presentation) error-message)
	    ;; don't update the client-value with invalid text. return
	    ;; NIL as the primary value.
	    (return-from test-validity (values nil provided-value)))))
      (setf (client-value presentation) provided-value)
      ;; now check if the lisp value is also valid.
      (setf (warning-message presentation)
            (multiple-value-bind (validp error-message)
                (validp presentation)
              (if validp nil error-message)))
      ;; return T as the primary value only if the value is valid.
      (values (not (warning-message presentation)) provided-value))))

