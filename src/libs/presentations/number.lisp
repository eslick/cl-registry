(in-package :registry)

(defclass number-presentation (input-based-field-presentation)
  ((precision :accessor precision :initarg :precision :initform nil)))

(define-lisp-value-getter number-presentation (client-value)
  (or (when (> (length client-value) 0)
	(cl-l10n:parse-number client-value))
      :none))

(define-lisp-value-setter number-presentation (number precision)
  (if (equal number :none)
      ""
      (if precision 
	  (format nil "~,vF" precision number)
	  (format nil "~D" number))))

;;;; * integer type

(defclass integer-presentation (number-presentation)
  ())

(define-lisp-value-getter integer-presentation (client-value)
  (or (parse-integer client-value) :none))

(define-lisp-value-setter integer-presentation (integer)
  (format nil "~D" integer))

;;;; * range integer type

(defclass range-integer-presentation (integer-presentation)
  ())

(define-lisp-value-getter range-integer-presentation (client-value)
  (if (equal client-value "") :none
      (let ((integers (cl-ppcre:all-matches-as-strings "[0-9]+" client-value)))
	(case (length integers)
	  (1 (call-next-method))
	  (2 (cons (parse-integer (first integers))
		   (parse-integer (second integers))))
	  (t (error "Unrecognized range specification in ~A" client-value))))))

(define-lisp-value-setter range-integer-presentation (integer-pair)
  (if (consp integer-pair) 
      (format nil "~D-~D" (car integer-pair) (cdr integer-pair))
      (call-next-method)))

;;;; * range limited integers

(defclass number-validator (web-field-validator)
  ()
  (:documentation "Super class for all validotars which compare a lisp-value to a number."))

(defmethod lisp-validate :around ((validator number-validator) (lisp-value t))
  (if (and (not (numberp lisp-value)) (not (equal lisp-value :none)))
      (fail-validation "~S is not a number." lisp-value)
      (call-next-method)))

(defmethod client-validate ((validator number-validator) (client-value string))
  (if (plusp (length client-value))
    (handler-case
	(cl-l10n:parse-number client-value)
      (parse-error (c)
	(declare (ignore c))
	(fail-validation "~S is not a number" client-value)))
    (fail-validation "No number provided.")))

(defclass min-value-validator (number-validator)
  ((min-value :accessor min-value :initarg :min-value :initform nil)))

(defmethod lisp-validate ((validator min-value-validator) (lisp-value number))
  (if (<= (min-value validator) lisp-value)
      t
      (fail-validation "~S is greater than or equal to ~S." lisp-value (min-value validator))))

(defclass max-value-validator (number-validator)
  ((max-value :accessor max-value :initarg :max-value)))

(defmethod lisp-validate ((validator max-value-validator) (lisp-value number))
  (if (>= (max-value validator) lisp-value)
      t
      (fail-validation "~S is less than or equal to ~S." lisp-value (max-value validator))))
