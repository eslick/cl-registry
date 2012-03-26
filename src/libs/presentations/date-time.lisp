(in-package :registry)

;;; Using CL-L10N:PARSE-TIME patterns selectively

(defvar *parse-time-args-alist* nil)

(defvar *parse-time-args* nil)

(defparameter *patterns* 
  '((:date-month-day . "MM/dd")
    (:date-full . "yyyy/MM/dd")
    (:date-month-year . "yyyy/MM")))

(defun get-pattern (key)
  (or (assoc-get key *patterns*)
      (assoc-get :date-full *patterns*)))

(defun add-parse-time-args (key &rest args)
  (setq key (as-keyword key))
  (check-type key symbol)
  (let ((old (assoc (as-keyword key) *parse-time-args-alist*)))
    (aif old
	 (setf (cdr old) args)
	 (push (cons key args) *parse-time-args-alist*))))

(defgeneric datetime-presentation-parse-time-args (presentation)
  (:documentation "Return a list of args to be passed to CL-L10N:PARSE-TIME
when parsing date/time strings for PRESENTATION"))

(defmacro with-datetime-presentation-parse-time-args ((presentation) &body body)
  `(let ((*parse-time-args*
	  (or (datetime-presentation-parse-time-args ,presentation)
	      *parse-time-args*)))
     ,@body))

;;;; * datetime type

;; NOTE: Need to handle current time from the client
;; NOTE: Need to switch current-locale based on client

(defclass datetime-presentation (input-based-field-presentation)
  ((show-date-p :accessor show-date-p :initform t :initarg :show-date-p)
   (show-time-p :accessor show-time-p :initform t :initarg :show-time-p)
   (parse-time-patterns-key :accessor parse-time-patterns-key :initarg :parse-time-patterns-key :initform nil)))

(define-lisp-value-getter datetime-presentation (client-value)
  (with-datetime-presentation-parse-time-args (datetime-presentation)
    (or (when (equal client-value "unknown") :unknown)
	(when (equal client-value "present") :present)
	(parse-datetime client-value)
	:none)))

(define-lisp-value-setter datetime-presentation (lisp-value show-time-p show-date-p parse-time-patterns-key)
  (cond ((eq lisp-value :none) nil)
	((eq lisp-value :unknown) "unknown")
	((eq lisp-value :present) "present")
        (lisp-value (print-datetime lisp-value 
				    :show-time-p show-time-p
				    :show-date-p show-date-p
				    :pattern (get-pattern parse-time-patterns-key)))
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

(defmethod datetime-presentation-parse-time-args ((presentation datetime-presentation))
  (aif (parse-time-patterns-key presentation)
       (cdr (assoc it *parse-time-args-alist*))))

(defmethod validp ((presentation datetime-presentation))
  (with-datetime-presentation-parse-time-args (presentation)
    (call-next-method)))

(defmethod update-presentation ((presentation datetime-presentation) args)
  (declare (ignorable args))
  (with-datetime-presentation-parse-time-args (presentation)
    (call-next-method)))

;;;; * date type

(defclass date-presentation (datetime-presentation)
  ()
  (:default-initargs :show-time-p nil))

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
     (2
      (with-datetime-presentation-parse-time-args (date-range-presentation)
	(cons (or (parse-date (aref dates 0) *parse-time-args*)
		  ;;(apply #'cl-l10n:parse-time (aref dates 0) *parse-time-args*)
		  (when (equal (aref dates 0) "unknown") :unknown)
		  (when (equal (aref dates 0) "present") :present))
	      (or (parse-date (aref dates 1) *parse-time-args*)
		  ;;(apply #'cl-l10n:parse-time (aref dates 1) *parse-time-args*)
		  (when (equal (aref dates 1) "unknown") :unknown)
		  (when (equal (aref dates 1) "present") :present)))))
     (t (error "Unrecognized range specification in ~A" client-value)))))

(define-lisp-value-setter date-range-presentation (date-pair parse-time-patterns-key)
  (if (consp date-pair)
      (let ((date1 (car date-pair))
	    (date2 (cdr date-pair)))
	(with-string-stream (stream)
	  (if (keywordp date1)
	      (princ (string-downcase (symbol-name date1)) stream)
	      (render-date stream date1 (get-pattern parse-time-patterns-key)))
	  (princ " to " stream)
	  (if (keywordp date2)
	      (princ (string-downcase (symbol-name date2)) stream)
	      (render-date stream date2 (get-pattern parse-time-patterns-key)))))
      (progn (call-next-method)
             (return-from lisp-value date-pair))))

(defclass date-range-validator (non-nil-validator)
  ()
  (:default-initargs :error-message "Unable to determine date."))

(defmethod client-validate ((validator date-range-validator) (client-value string))
  t)

(defmethod lisp-validate ((validator date-range-validator) (lisp-value cons))
  t)

;;; Date presentation hints for display in forms

(defgeneric date-presentation-hint-for-locale (presentation locale)
  (:method (presentation (locale null))
    (date-presentation-hint-for-locale presentation (cl-l10n:current-locale)))
  (:method (presentation (locale string))
    (date-presentation-hint-for-locale presentation (cl-l10n:locale locale)))
  (:documentation "Compute a date presentation hint for PRESENTATION and LOCALE"))

(defgeneric date-presentation-hint-for-user (presentation user)
  (:documentation "Compute a date presentation for PRESENTATION and USER in their preferred locale if known"))

(defmethod date-presentation-hint-for-user (presentation (user t))
  ;; Note method signature avoids forward references to USER class 
  ;; This approach will allow to redefine as a class method later
  (let ((current-locale (user-locale (get-user user))))
    (date-presentation-hint-for-locale presentation current-locale)))

(defmethod date-presentation-hint-for-user (presentation (user null))
  (date-presentation-hint-for-user presentation (current-user)))

;; Cache some date presentation hints to avoid trouble

(defvar *date-presentation-hint-for-locale-alist*
  '(
    ;; Not caching en_US now that we transform the presentation hint depending on desired input pattern
    ;; ("en_US" "mm/dd/yyyy")
    ;; Handle some cases with special date separator chars
    ;; Japanese
    ("ja_JP" "yyyy-mm-dd"))
  "Association list of (LOCALE FMT) where LOCALE is a locale string and FMT is the output format spec string")

(defun date-presentation-hint-for-locale-guess (&key pattern (locale (cl-l10n:current-locale)))
  "Guess input date presentation hint from output format for LOCALE"
  (let* ((locale-obj (cl-l10n:locale locale))
	 (locale-name (cl-l10n:locale-name locale-obj)))
    (cond
      ;; First check the association list for screw cases
      ((second (assoc locale-name *date-presentation-hint-for-locale-alist* :test #'string-equal)))
      ;; Guess by permuting date print string specification
      ;; TODO: Fix this! (ISE 2011)
      ((let* ((fmt "%F"));;(cl-l10n:locale-d-fmt locale-obj)))
	 (cond
	   ;; See the CL-L10N manual for definitions of % directives
	   ((string= fmt "%D") (if (eq pattern ':date-month-day) "mm/yyyy or mm/dd/yyyy or 'present'" "mm/dd/yyyy or 'unknown'"))
	   ((string= fmt "%F") (if (eq pattern ':date-month-year) "yyyy/mm or yyyy/mm/dd or 'present'" "yyyy/mm/dd or 'unknown'"))
	   ;; Most common date separators are slash, dot, and dash
	   ;; This process catches typical cases: %m/%d/%Y, %d/%m/%Y, etc.
	   ((let ((work fmt))
	      (setq work (cl-ppcre:regex-replace "%d" work "dd"))
	      (setq work (cl-ppcre:regex-replace "%m" work "mm"))
	      (setq work (cl-ppcre:regex-replace "%[Yy]" work "yyyy"))
	      ;; Did we replace all variable fields?
	      (when (not (cl-ppcre:scan "%" work))
		(cl-ppcre:regex-replace
		 "^[/.\-]"	    ;strip left-over leading separator
		 (case pattern
		   ;; show variant with optional part removed
		   (:date-month-day
		    (format nil "~A or ~A" (cl-ppcre:regex-replace "[/.\-]*yyyy" work "") work))
		   (:date-month-year
		    (format nil "~A or ~A" (cl-ppcre:regex-replace "[/.\-]*dd" work "") work))
		   (otherwise work))
		 ""))))))))))

(defmethod date-presentation-hint-for-locale ((presentation datetime-presentation) locale)
  (date-presentation-hint-for-locale-guess :locale locale :pattern (parse-time-patterns-key presentation)))

(defmethod date-presentation-hint-for-locale ((presentation date-range-presentation) locale)
  (format nil  "date1 to date2 (~A)"
	  (date-presentation-hint-for-locale-guess :locale locale :pattern (parse-time-patterns-key presentation))))

(defun emit-html-locale-date-format (presentation)
  (with-html
    (:span :class "date-presentation-format-label"
	   (:span :class "question-help"
		 (str (date-presentation-hint-for-user presentation nil))))))

;;; More methods

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

(define-lisp-value-getter time-presentation (client-value)
  (cond ((= (length client-value) 0) :none)
	(t client-value)))

(define-lisp-value-setter time-presentation (lisp-value show-time-p show-date-p parse-time-patterns-key)
  (cond ((eq lisp-value :none) nil)
        (t lisp-value)))

;;;; * validator

(defclass datetime-validator (non-nil-validator)
  ()
  (:default-initargs :error-message "Unable to recognize date."))

(defmethod client-validate ((validator datetime-validator) (client-value string))
  (handler-case
      (or (equal client-value "unknown") 
	  (equal client-value "present")
	  ;; (apply #'cl-l10n:parse-time client-value :error-on-mismatch t *parse-time-args*))
	  (parse-datetime client-value :fail-on-error t))
    (local-time::invalid-timestring (c)
      (declare (ignore c))
      (fail-validation "Invalid date and/or time"))
    (error (c)
      (declare (ignore c))
      (fail-validation (format nil "~S is not a valid date/time" client-value)))))

(defmethod lisp-validate ((validator datetime-validator) (lisp-value cons))
  t)

;;; Parse-time patterns (see top of compilation unit)

(in-package :cl-l10n)

(registry::add-parse-time-args
 ':date-full
 ;; These are simpler versions of the date-only patterns in the default set
 :patterns '(( month (date-divider) day (date-divider) year )
	     ( day (date-divider) month (date-divider) year )
	     (year (date-divider) month (date-divider) day )
	     ))

(registry::add-parse-time-args
 ':date-month-day
 ;; These are simpler versions of the date-only patterns in the default set
 :patterns '(( month (date-divider) day (date-divider) year )
	     ( day (date-divider) month (date-divider) year )
	     ( month (date-divider) day )
	     (year (date-divider) month (date-divider) day )
	     (month (date-divider) year )
	     (year (date-divider) month )
	     ))

(registry::add-parse-time-args
 ':date-month-year
 :default-day 1
 ;; These are simpler versions of the date-only patterns in the default set
 :patterns  '(( month (date-divider) day (date-divider) year )
	      ( day (date-divider) month (date-divider) year )
	      (month (date-divider) year )
	      ( month (date-divider) day )
	      (year (date-divider) month (date-divider) day )
	      (year (date-divider) month )
	      ))

