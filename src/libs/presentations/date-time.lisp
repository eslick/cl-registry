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

(defgeneric date-presentation-hint-for-locale (presentation locale)
  (:documentation "Compute a date presentation hint for PRESENTATION and LOCALE"))

(defmethod date-presentation-hint-for-locale (presentation (locale null))
  "Compute a date presentation hint for PRESENTATION and current user's LOCALE"
  (let ((current-locale (cl-l10n:locale-name (user-locale (current-user)))))
    (check-type current-locale string)
    (date-presentation-hint-for-locale presentation current-locale)))

(defvar *date-presentation-hint-for-locale-alist*
  '(
    ;; Cache this
    ("en_US" "mm/dd/yyyy")
    ;; Handle some cases with special date separator chars
    ;; Japanese
    ("ja_JP" "yyyy-mm-dd"))
  "Association list of (LOCALE FMT) where LOCALE is a locale string and FMT is the output format spec string")

(defun date-presentation-hint-for-locale-guess (&optional (locale (cl-l10n:locale-name (cl-l10n:current-locale))))
  "Guess input date presentation hint from output format for LOCALE"
  (cond
    ;; First check the association list for common / screwy cases
    ((second (assoc locale *date-presentation-hint-for-locale-alist* :test #'string-equal)))
    ;; Guess by permuting date print string specification
    ((let* ((fmt (cl-l10n:locale-d-fmt locale)))
       (cond
	 ;; This catches typical: %m/%d/%Y, %d/%m/%Y, etc.
	 ((let ((work fmt))
	    (and (setq work (cl-ppcre:regex-replace "%d" work "dd"))
		 (setq work (cl-ppcre:regex-replace "%m" work "mm"))
		 (setq work (cl-ppcre:regex-replace "%[Yy]" work "yyyy"))
		 ;; Consider replacing "-" with "/"
		 ;;(setq work (cl-ppcre:regex-replace-all "-" work "/"))
		 ;; Did we replace all variable fields?
		 (not (cl-ppcre:scan "%" work))
		 ;; Returns
		 work)))
	 ;; If the CL-L10N doc is correct, %D always prints as mm/dd/yy
	 ((string= fmt "%D") "mm/dd/yyyy")
	 ;; Our parser probably won't work with yyyy-mm-dd but this gives us a hint as to order
	 ((string= fmt "%F") "yyyy/mm/dd"))))))
	 
(defmethod date-presentation-hint-for-locale ((fmt string) (locale string))
  (format nil fmt (date-presentation-hint-for-locale-guess locale)))

(defmethod date-presentation-hint-for-locale ((presentation date-presentation) locale)
  (date-presentation-hint-for-locale "(~A)" locale))

(defmethod date-presentation-hint-for-locale ((presentation date-range-presentation) locale)
  (date-presentation-hint-for-locale "(~A to ~:*~A)" locale))

(defun emit-html-locale-date-format (presentation)
  (with-html
    (:span :class "date-presentation-format-label"
	   (:span :class "question-help"
		 (str (date-presentation-hint-for-locale presentation nil))))))

(defmethod render-presentation-editable :after ((presentation date-presentation))
  (let ((parse-output-span-id (genweb-field-name)))
    (with-html
      " "
      (str (emit-html-locale-date-format presentation))
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
