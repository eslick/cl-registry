(in-package :registry)

(defclass measurement-presentation (number-presentation)
  ((display-unit :accessor display-unit :initform ""
		 :initarg :display-unit)
   (canonical-unit :accessor canonical-unit :initform nil
		   :initarg :canonical-unit
		   :documentation "Convert from the user specified
		   unit to this one.")
   (measurement :accessor measurement :initform nil
		:initarg :measurement
		:documentation "A keyword, such as :length, :weight,
		etc.")
   (quantity :accessor quantity :initform ""))
  (:default-initargs :precision 2))

(defmethod render-presentation-editable ((presentation measurement-presentation))
  (with-html
    (:span
     (call-next-method)
     (:div :class "question-help" (str (display-unit presentation))))))

(define-lisp-value-getter measurement-presentation (client-value canonical-unit display-unit)
  (dbind (&optional client-number unit) 
      (cl-ppcre:split '(:char-class #\Space) client-value)
    (when (and client-number (> (length client-number) 0))
      (handler-case 
	  (let ((lisp-number (cl-l10n:parse-number client-number)))
	    (values (convert-to-canonical lisp-number
					  (or unit
					      display-unit
					      (get-locale-unit canonical-unit)))
		    canonical-unit))
	(error () 0)))))
	 
(define-lisp-value-setter measurement-presentation (number canonical-unit display-unit precision)
  ;; maybe do conversion then call next method
  (let* ((unit (or display-unit
		   (get-locale-unit canonical-unit)
		   canonical-unit)))
    (aif (ignore-errors 
	   (convert-from-canonical number unit))
	 (if precision
	     (format nil "~,vF" precision it)
	     (format nil "~D" it))
	"")))

#||
(defun unit-conversion (num current target)
  (if (or (equal current target)
	  (null target))
      num
      (let ((spec (convert-unit-fn current target)))
	(typecase spec
	  (function (funcall spec num))
	  (number (* num spec))
	  (null (error "No conversion"))))))

(defparameter *unit-conversions*
  `(("kg" ("lbs"     2.2))
    ("cm" ("inches"  0.39))
    ("km" ("miles"   0.6213712))
    ("m"  ("feet"    3.2808399 ))
    ("l"  ("gallons" 0.264172))
    ("C"  ("F" ,(lambda (f) (* (/ 5 9) (- f 32)))))
    ("F"  ("C" ,(lambda (c) (* (/ 9 5) (+ c 32)))))))

(defparameter *unit-aliases*
  '(("km" "kilometer" "kilometers")
    ("kg" "kilogram" "kilograms")
    ("cm" "centimeter" "centimeters")
    ("m"  "meter" "meters")
    ("l"  "liter" "liters")
    ("C"  "celsius" "centigrade")
    ("F"  "fahrenheit")))

(defun convert-unit-fn (current-unit target-unit)
  (aif (find current-unit *unit-conversions* :key 'first :test 'equal)
       (awhen (find target-unit (cdr it) :key 'first :test 'equal)
	 (second it))
       (aif (find target-unit *unit-conversions* :key 'first :test 'equal)
	    (awhen (find current-unit (cdr it) :key 'first :test 'equal)
	      (/ 1 (second it))))))
||#

(defparameter *canonical-units*
  '((:weight . "kg")
    (:dosage . "mg")
    (:length . "cm")
    (:bmi . "kg/m2")
    (:years . "years")
    (:minutes . "minutes")
    (:seconds . "seconds")
    (:temperature . "C")
    (:volume . "liters")
    (:fev1 . "liters")))

(defun canonical-unit-for-measurement (measurement)
  (cdr (assoc measurement *canonical-units*)))

(defparameter *unit-to-canonical*
  `(("kg" . 1)
    ("mg" . 1)
    ("cm" . 1)
    ("kg/m2" . 1)
    ("years" . 1)
    ("minutes" . 1)
    ("seconds" . 1)
    ("liters" . 1)
    ("C" . 1)
    ("mcg" . .001) ;; to mg
    ("inches" . 2.54) ;; to cm
    ("lbs" . 0.45359237) ;; to kg
    ("feet" . 30.5) ;; to cm
    ("gallons" . 3.785) ;; to liters
    ("m" . .01) ;; to cm
    ("meters" . .01) ;; to cm
    ("F" . ,(lambda (f) (* (/ 5 9) (- f 32))))))

(defun unit-to-canonical-fn (unit)
  (cdr (assoc unit *unit-to-canonical* :test #'string-equal)))

(defun convert-to-canonical (n unit)
  (let ((spec (unit-to-canonical-fn unit)))
    (typecase spec
      (function (funcall spec n))
      (number (* n spec))
      (null (error "No conversion")))))

(defun known-unit-p (unit)
  (find unit *unit-to-canonical* :key #'car :test #'string-equal))

(defparameter *unit-from-canonical*
  `(("kg" . 1)
    ("mg" . 1)
    ("cm" . 1)
    ("kg/m2" . 1)
    ("years" . 1)
    ("minutes" . 1)
    ("seconds" . 1)
    ("liters" . 1)
    ("m" . 1)
    ("meters" . 1)
    ("mcg" . 1000) ;; from mg
    ("inches" . 0.3937) ;; from cm
    ("gallons" . 0.264172) ;; from liters
    ("lbs" . 2.2046226) ;; from kg
    ("feet" . 0.0328084) ;; from cm
    ("F" . ,(lambda (c) (+ (* (/ 9 5) c) 32)))
    ("C" . 1)))

(defun unit-from-canonical-fn (unit)
  (cdr (assoc unit *unit-from-canonical* :test #'string-equal)))

(defun convert-from-canonical (n unit)
  (let ((spec (unit-from-canonical-fn unit)))
    (typecase spec
      (function (funcall spec n))
      (number (* n spec))
      (null (error "No conversion")))))

(defparameter *us-units*
  '((:weight . "lbs")
    (:dosage . "mg")
    (:length . "inches")
    (:bmi . "kg/m2")
    (:volume . "gallons")
    (:years . "years")
    (:minutes . "minutes")
    (:seconds . "seconds")
    (:temperature . "F")
    (:fev1 . "liters")
    (:dlco . "ml/min/mmHg")))

(defparameter *si-units*
  '((:length . "m")
    (:dosage . "mg")
    (:weight . "kg")
    (:bmi . "kg/m2")
    (:volume . "liters")
    (:years . "years")
    (:minutes . "minutes")
    (:seconds . "seconds")
    (:temperature . "C")
    (:fev1 . "liters")
    (:dlco . "mmol/min/kPa")))

(defun get-locale-unit (canonical-unit)
  (let* ((locale (if (boundp 'hunchentoot:*session*)
		   (user-locale (current-user))
		   (cl-l10n:locale "en_US")))
	 (locale-name (cl-l10n:locale-name locale))
	 (measurement (car (rassoc canonical-unit *canonical-units*
				   :test #'equal))))
    (cond
      ((string= locale-name "en_US")
       (cdr (assoc measurement *us-units*)))
      (t
       (cdr (assoc measurement *canonical-units*))))))
	 
(defclass measurement-validator (non-nil-validator)
  ((measurement :accessor measurement :initarg :measurement
		:initform nil
		:documentation "A keyword such as :length, :weight,
		etc."))
  (:default-initargs :error-message "Unable to determine value and unit."))

(defmethod client-validate ((validator measurement-validator) (client-value string))
  (dbind (&optional client-number unit) 
      (cl-ppcre:split '(:char-class #\Space) client-value)
    (if client-number
        (progn
          (handler-case
              (cl-l10n:parse-number client-number)
            (cl-l10n::parse-error (c)
              (return-from client-validate
                (fail-validation (cl-l10n::reason c)))))
          (when unit
            (let ((canonical-unit (canonical-unit-for-measurement
                                   (measurement validator))))
              (unless (or (null unit)
                          (and canonical-unit
                               (unit-from-canonical-fn unit)))
                (return-from client-validate
                  (fail-validation "Specified unit '~A' unknown." unit)))))
            ;; If we get here, then there is a number, and if there's a
            ;; unit, we know how to convert it into the canonical unit.
          (values t))
        (fail-validation "No number provided."))))
