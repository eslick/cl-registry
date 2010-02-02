(in-package :registry)

;;;; * datetime type

;; NOTE: Need to handle current time from the client
;; NOTE: Need to switch current-locale based on client

(defclass datetime-presentation (input-based-field-presentation)
  ((show-date-p :accessor show-date-p :initform t :initarg :show-date-p)
   (show-time-p :accessor show-time-p :initform t :initarg :show-time-p)))

(define-lisp-value-getter datetime-presentation (client-value)
  (or (cl-l10n:parse-time client-value) :none))

(define-lisp-value-setter datetime-presentation (lisp-value show-time-p show-date-p)
  (cond ((eq lisp-value :none) nil)
        (lisp-value (with-string-stream (stream)
                      (cl-l10n:print-time lisp-value
                                          :show-date show-date-p
                                          :show-time show-time-p
                                          :locale (user-locale (current-user))
                                          :stream stream)))
        (t "")))

(defmethod render-presentation-editable ((presentation datetime-presentation))
  (with-html
    (:input :type "input"
            :name (query-name presentation)
            :id (dom-id presentation)
            :class (css-class presentation)
            :style (css-style presentation)
            :value (client-value presentation)
            :maxlength (maxlength presentation)
            :onchange (on-change-validation presentation))))

(defclass date-presentation (datetime-presentation)
  ()
  (:default-initargs :show-time-p nil))

(defmethod render-presentation-editable :after ((presentation date-presentation))
  (let ((parse-output-span-id (genweb-field-name)))
    (with-html
      " "
      (cond
        ((string-starts-with "en_US" (cl-l10n:locale-name (user-locale (current-user))))
         (str
          (if (typep presentation 'date-range-presentation)
              #!"(mm/dd/yyyy[ to mm/dd/yyyy])"
              #!"(mm/dd/yyyy)")))
        #| ((string-starts-with "ja" (cl-l10n:locale-name (user-locale (current-user)))) (str #!"(yyyy/mm/dd)")) |#
        (t
         (str
          (if (typep presentation 'date-range-presentation)
              #!"(dd/mm/yyyy[ to dd/mm/yyyy)"
              #!"(dd/mm/yyyy)"))))
      (:span :id parse-output-span-id  ""))))

      ;; should use parenscript here, not adding a dependency this
      ;; late in the game.
;;      (js-for-date-parsing (dom-id presentation) parse-output-span-id
;;                           (string-starts-with "en" (cl-l10n:locale-name (user-locale (current-user))))))))

(defun js-for-date-parsing (dom-id parse-output-span-id en-p)
  (with-html
    (:script
     (cl-who:fmt "$('~A').onchange = generateDateUpdater($('~A'), $('~A'), ~A);"
                 dom-id dom-id ;; sic
                 parse-output-span-id (if en-p "true" "false")))))

(defclass time-presentation (datetime-presentation)
  ()
  (:default-initargs :show-date-p nil))

;;;; * validator

(defclass datetime-validator (non-nil-validator)
  ()
  (:default-initargs :error-message "Unable to determine date."))

(defmethod client-validate ((validator datetime-validator) (client-value string))
  (handler-case
      (cl-l10n:parse-time client-value :error-on-mismatch t)
    (cl-l10n:parser-error (c)
      (fail-validation (cl-l10n::reason c)))
    (error (c)
      (declare (ignore c))
      (fail-validation (format nil "~S is not a valid date/time" client-value)))))

(defmethod lisp-validate ((validator datetime-validator) (lisp-value cons))
  t)
;;;; * a range of dates

(defclass date-range-presentation (date-presentation)
  ()
  (:default-initargs :css-style "width: 24em"))


(define-lisp-value-getter date-range-presentation (date-presentation client-value)
  (mvbind (string dates)
      (cl-ppcre:scan-to-strings "(.+)[\\s]+to[\\s]+(.+)" client-value)
    (declare (ignorable string))
    (unless dates
      (multiple-value-setq (string dates)
        (cl-ppcre:scan-to-strings "(.+)" client-value)))
   (case (length dates)
     (0 :none)
     (1 (call-next-method))
     (2 (cons (cl-l10n:parse-time (aref dates 0))
	      (cl-l10n:parse-time (aref dates 1))))
     (t (error "Unrecognized range specification in ~A" client-value)))))

(define-lisp-value-setter date-range-presentation (date-pair)
  (if (consp date-pair)
      (with-string-stream (stream)
	(cl-l10n:print-time (car date-pair) :show-date t 
			    :show-time nil :stream stream)
	(princ " to " stream)
	(cl-l10n:print-time (cdr date-pair) :show-date t 
			    :show-time nil :stream stream))
      (progn (call-next-method)
             (return-from lisp-value date-pair))))

(defclass date-range-validator (non-nil-validator)
  ()
  (:default-initargs :error-message "Unable to determine date."))

(defmethod client-validate ((validator date-range-validator) (client-value string))
  t)

(defmethod lisp-validate ((validator date-range-validator) (lisp-value cons))
  t)
