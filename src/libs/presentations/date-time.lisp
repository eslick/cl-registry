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
  (if lisp-value
      (with-string-stream (stream)
	(cl-l10n:print-time lisp-value
			    :show-date show-date-p
			    :show-time show-time-p
			    :locale (user-locale (current-user))
			    :stream stream))
      ""))

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
      (cond
        ((string-starts-with "en_US" (cl-l10n:locale-name (user-locale (current-user))))
         (str #!"(mm/dd/yyyy)"))
        #| ((string-starts-with "ja" (cl-l10n:locale-name (user-locale (current-user)))) (str #!"(yyyy/mm/dd)")) |#
        (t
         (str #!"(dd/mm/yyyy)")))
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

;;;; * a range of dates

(defclass date-range-presentation (date-presentation)
  ())

(define-lisp-value-getter date-range-presentation (date-presentation client-value)
  (mvbind (string dates)
     (cl-ppcre:scan-to-strings "(.*)[\s]+to[\s]+(.*)" client-value)
   (declare (ignore string))
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
      (call-next-method)))

(defclass datetime-validator (non-nil-validator)
  ()
  (:default-initargs :error "Unable to determine date."))

(defmethod client-validate ((validator datetime-validator) (client-value string))
  (handler-case
      (cl-l10n:parse-time client-value :error-on-mismatch t)
    (cl-l10n:parser-error (c)
      (fail-validation (cl-l10n::reason c)))))
