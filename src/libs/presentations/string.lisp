(in-package :registry)

(defclass input-based-field-presentation (web-field-presentation)
  ((maxlength :accessor maxlength :initarg :maxlength :initform nil)))

(defmethod render-presentation-editable ((presentation input-based-field-presentation))
  (let ((client-value (client-value presentation)))
    (when (null client-value)
      (setf client-value ""))
    (with-html
      (:input :type "text" :name (query-name presentation)
	      :id (dom-id presentation)
	      :class (css-class presentation)
	      :style (css-style presentation)
	      :value client-value
	      :maxlength (maxlength presentation)
	      ;; remember to stick the validators in here
	      :onchange (on-change-validation presentation)))))

;;;; * simple string type

(defclass string-presentation (input-based-field-presentation)
  ())

(define-lisp-value-getter string-presentation (client-value)
  client-value)

(define-lisp-value-setter string-presentation (new-value)
  new-value)

;;;; ** short-string

(defclass short-string-presentation (string-presentation)
  ())

;;;; ** password (hidden short string)

;; nb: weblocks also has a password-presentation, we could probably
;; find a better name for this one.
(defclass password-field-presentation (short-string-presentation)
  ())

(defmethod render-presentation-editable ((presentation password-field-presentation))
  (with-html
    (:input :type "password"
            :name (query-name presentation)
            :id (dom-id presentation)
            :class (css-class presentation)
            :style (css-style presentation)
            :value (client-value presentation)
            :maxlength (maxlength presentation)
            :onchange (on-change-validation presentation))))

;;;; ** text area

(defclass text-area-presentation (string-presentation)
  ((rows :reader rows :initarg :rows :initform nil)
   (cols :reader cols :initarg :cols :initform nil)))

(defmethod render-presentation-editable ((presentation text-area-presentation))
  (with-html
    (:textarea :name (query-name presentation)
               :id (dom-id presentation)
               :class (css-class presentation)
               :style (css-style presentation)
               :onchange (on-change-validation presentation)
               :rows (rows presentation)
               :cols (cols presentation)
      (str (client-value presentation)))))

;;;; ** regexp validator

(defclass string-validator (web-field-presentation)
  ()
  (:documentation "Super class for all validators which compare a lisp-value to a string."))

(defmethod lisp-validate :around ((validator string-validator) (lisp-value t))
  (if (not (stringp lisp-value))
      (fail-validation "~S is not a string." lisp-value)
      (call-next-method)))

(defclass regexp-validator (string-validator)
  ((regexp :initarg :regexp :reader regexp
           :documentation "A string or a cl-ppcre:scanner object.")))

(defmethod lisp-validate ((regexp regexp-validator) (lisp-value string))
  (if (cl-ppcre:scan (regexp regexp) lisp-value)
      (values t)
      (fail-validation "~S does not match the regexp ~S." lisp-value (regexp regexp))))

(defclass min-length-validator (string-validator)
  ((min-length :reader min-length :initarg :min-length))
  (:documentation "Checks that the string is at least MIN-LENGTH chars
  long."))

(defmethod lisp-validate ((min-length-validator min-length-validator) (lisp-value string))
  (if (<= (length lisp-value) (min-length min-length-validator))
      t
      (fail-validation "~S is only ~D chars long but needs to be at least ~D long."
                       lisp-value (length lisp-value) (min-length min-length-validator))))
