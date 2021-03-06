(in-package :registry)

(registry-proclamations)

;; ===========================================================
;;  Survey question model
;; ===========================================================

;; Prompt, type, view, relation
;; => validation, extraction

(defmodel question (fulltext-mixin user-translation-mixin published-data-mixin)
  (;; Prompt
   (name :accessor question-name :initarg :name :index t)
   (number :accessor question-number :initarg :number :initform nil)
   (prompt :accessor question-prompt :initarg :prompt :index t) ;; statisfy via fulltext!
   (prompt-format :accessor question-prompt-format :initarg :prompt-format :initform nil)
   (question-help :accessor question-help :initarg :help :initform "")
   (data-help :accessor question-data-help :initarg :data-help :initform "")
   ;; Type and view
   (data-type :accessor question-data-type :initarg :data-type :initform :string)
   (data-subtype :accessor question-data-subtype :initarg :data-subtype :initform nil)
   (data-constraint :accessor question-data-constraint :initarg :data-constraint :initform nil)
   (view-type :accessor question-view-type :initarg :view-type :initform :auto)
   (choices :accessor question-choices :initarg :choices :initform nil
	    :documentation "An alist of human-value . lisp-value")
   (parent :accessor parent :initarg :parent :initform nil)
   ;; Properties
   (required :accessor question-required-p :initarg :required :initform nil)
   (hipaa-id-p :accessor question-hipaa-id-p :initarg :hipaa-id-p :initform nil
               :documentation "Non-nil if question stores HIPAA identifier"))
  (:documentation "The base class for survey and diary questions."))

(defmethod shared-initialize :after ((instance question) slot-names &rest args &key
				     (data-type ':string data-type-supp-p) (hipaa-id-p nil hipaa-id-supp-p))
  (declare (ignore args slot-names hipaa-id-p))
  (when (and (not hipaa-id-supp-p) data-type-supp-p (eq data-type ':date))
    (setf (slot-value instance 'hipaa-id-p) t)))

(defmethod fulltext-fields ((instance question))
  '(name prompt question-help))

(defmethod print-object ((inst question) stream)
  (format stream "#<QUESTION (~A) '~A'>"
	  (mid inst)
	  (when (slot-boundp inst 'prompt)
	    (question-prompt inst))))

(defmethod translate-fields ((obj question))
  '(prompt choices question-help data-help))

(defun diary-question-p (question)
  (diary-group-p (parent question)))

(defmethod humanize-name ((inst question))
  (let ((words (split-alphanumeric-string (question-prompt inst))))
    (if (> (length words) 5)
	(unsplit-words (subseq words 0 5))
	(question-prompt inst))))

(defmethod attributize-name ((inst question))
  (attributize-name (question-name inst)))

(defun get-question (question-ref)
  (assert-body-type 'question
    (typecase question-ref
      (number (get-model 'question question-ref))
      (question question-ref)
      (t (get-instance-by-value 'question 'prompt question-ref)))))

(defmethod drop-instance ((inst question))
  (when (drop-question inst)
    (call-next-method)))

(defun drop-question (question &key (interactive t))
  (when (or (not interactive)
	    (let ((answers (length (get-instances-by-value 'answer 'question question))))
	      (if (eq answers 0) t
		  (when (yes-or-no-p "Question ~A has ~A associated answers, do you really want to delete this question?" question answers)
		    (when (yes-or-no-p "Are you really really sure?")
		      t)))))
    (mapcar #'drop-instance (get-instances-by-value 'answer 'question question))
    t))

(defmethod format-prompt ((inst question) &key (format nil format-supplied-p) (default-format "~A"))
  (let ((qnum (and (slot-boundp inst 'number) (question-number inst)))
	(qname (and (slot-boundp inst 'name) (question-name inst)))
	(qprompt
	 ;; Prompt slot may be translated into current language
	 ;; This is == (question-prompt inst)
	 (and (slot-boundp inst 'prompt)
	      (slot-value-translation inst 'prompt))))
    (cond
      ((null qprompt) "")
      ((ignore-errors
	 (format nil
		 (or (and format-supplied-p format) (question-prompt-format inst) default-format)
		 qprompt qnum qname qprompt)))
      (t
       ;; No formatting
       qprompt))))

(defun make-question (name &rest args &key (prompt name)
		      &allow-other-keys)
  "Shortcut for (APPLY MAKE-INSTANCE 'QUESTION :NAME NAME :PROMPT PROMPT ARGS)
  PROMPT defaults to NAME but can be overridden."
  (apply #'make-instance 'question :name name :prompt prompt args))

;; Publishing

(defmethod published-data-p ((instance question))
  (and (not (question-hipaa-id-p instance))
       (published-data-p (parent instance))))

;;
;; Question views 
;;

(defview question-form-view (:type form)
  (prompt :present-as (textarea :cols 60 :rows 2))
  (question-help :present-as (textarea :cols 60 :rows 6)))

(defview question-table-view (:type table)
  elephant::oid
  name
  (required :present-as predicate))

(defview question-data-view (:type data)
  prompt
  question-help)


;; Protocol for rendering in generic views

(defmethod results-title ((inst question))
  (question-prompt inst))

;; Search question structure

(defun map-questions-by-type (fn type)
  (map-class (lambda (question)
	       (when (eq (question-data-type question) type)
		 (funcall fn question)))
	     'question))

(defun parent-group (question)
  (cond ((equal (type-of question) 'question)
	 (parent-group (parent question)))
	(t nil)))

;;
;; The context of a question (group, etc)
;;

(defvar *question-context-cache* (make-hash-table :test #'eq))
(defvar *cache-timestamp* (get-universal-time))

(defmacro memorize-question-context (question &body form)
  (with-gensyms (key)
    `(let ((,key ,question))
       (when (> (get-universal-time) 
		(+ *cache-timestamp* 3600)) ;; clear every hour
	     (setf *cache-timestamp* (get-universal-time))
	     (clrhash *question-context-cache*))
       (aif-ret (gethash ,key *question-context-cache*)
	 (setf (gethash ,key *question-context-cache*)
	       (progn ,@form))))))

(defun question-context (question)
  "Returns any parent questions"
  (memorize-question-context question 
    (remove-nulls
     (cons question 
	   (let ((group (parent question)))
	     (aif (and group (find-activating-question (parent group) group))
		  (question-context it)
		  (list group (first (find-group-surveys group)))))))))

(defun related-questions-in-context (question &key published-only-p)
  (let ((related nil))
    (awhen (parent question)
      (setf related (set-difference
		     (select-if (if published-only-p #'published-data-p #'identity) (group-questions it))
		     (list question))))
    (when (< (length related) 10)
      (setf related 
	    (append related 
		    (filter-if (lambda (obj)
				 (cond
				   ((not (eq (type-of obj) 'question)))
				   ((and published-only-p (not (published-data-p obj))))))
			       (fulltext-search 
				(clean-query 
				 (question-prompt question)))))))
    related))
      
      
(defun clean-query (query)
  (unsplit-words (filter-if #'stopword-p (split "[^\\w]+" query))))

;; ============================================
;;  Question presentation support
;; ============================================

;; DATA TYPE

;; boolean         - t or nil
;; string          - string
;; number          - number
;; date            - universal time
;; range-number    - (cons number number) (two lines or open field?)
;; choice(s)       - (list of IDs)
;; comma-list      - list of <type> separated by commas
;; newline-list    - list of <type> separated by newlines

;; VIEW TYPE
;; :text-field - string/number/lists
;; :paragraph  - string/lists
;; :checkbox   - boolean
;; :radio      - boolean, choice
;; :dropdown   - choices
;; :selection  - choices
;; :date-select - date

;; :auto for type
			    
(defmethod make-presentation ((question question) id &rest initargs)
  (let ((type (presentation-type-for-question question))
	(args (list (list :prompt (format-prompt question)))))
    (when (eq type 'multiple-members-select-presentation)
      (push (list :multiple-hint-p t) args))
    (cond ((subtypep type 'member-choice-presentation)
	   (if (eq type 'number-radio-presentation)
	     (let ((pair (question-data-constraint question)))
	       (when (consp pair)
		 (push (list :low-value (car pair)
			     :high-value (cdr pair)) args)))
	     ;; number-radio-presentation generates its own choices
	     (push (list :choices (question-choices-translation question)) args))
	   (when (eq type 'member-select-presentation)
	     (push (list :empty-option-p t) args)))
	  ((eq type 'measurement-presentation)
	   (let* ((measurement (question-data-subtype question))
		  (canonical-unit (canonical-unit-for-measurement measurement)))
	     (push (list
		    :canonical-unit canonical-unit
		    :display-unit (get-locale-unit canonical-unit)
		    :validators (list (make-instance 'measurement-validator
						     :measurement measurement)))
		   args)))
	  ((subtypep type 'datetime-presentation)
	   (push (list :parse-time-patterns-key
		       (find (question-data-subtype question)
			     '(:date :date-full :date-month-year :date-month-day)))
		 args)))
    (let ((presentation 
	   (apply #'make-instance type 
		  :metadata question 
		  :css-class (with-standard-io-syntax
                               (format nil "question-presentation question-data-type-~A" (question-data-type question)))
		  (flatten1 (cons initargs args)))))
      (case type
        (number-presentation
         (push (make-instance 'number-validator) (validators presentation)))
        ((or datetime-presentation date-presentation)
         (push (make-instance 'datetime-validator) (validators presentation))))
      (setf (dom-id presentation) (query-name presentation))
      (aif (get-answer question (current-patient) id)
	   (setf (lisp-value presentation) (value it))
	   (when (subtypep (type-of presentation) 'boolean-presentation)
	     (setf (lisp-value presentation) :none)))
      presentation)))

(defun question-choices-translation (question &optional lang)
  (let ((choices (question-choices question))
        (translations (slot-value-translation question 'choices lang)))
    (loop for (string . lisp) in choices
         collect (cons (or (car (rassoc lisp translations :test #'equal)) string)
                       lisp))))

(defmacro q-match-case (q &body clauses)
  (with-gensyms (data view question)
    `(let* ((,question ,q)
	    (,data (question-data-type ,question))
	    (,view (question-view-type ,question)))
       (when (eq ,view :auto)
	 (setf ,view (default-view-for-type ,data)))
       (cond ,@(mapcar (lambda (clause)
			 (dbind ((d v) result) clause
				`((and (or (null ,d) (eq ,data ,d))
				       (or (null ,v) (eq ,view ,v)))
				  ,result)))
		       clauses)
	     (t (error "No mapping for ~A ~A." ,data ,view))))))

(defun presentation-type-for-question (question)
  (q-match-case question
    ((:boolean :checkbox)  'checkbox-boolean-presentation)
    ((:boolean :dropdown)  'dropdown-boolean-presentation)
    ((:boolean :radio)     'radio-boolean-presentation)
    ((:string :text-field) 'short-string-presentation)
    ((:string :paragraph)  'text-area-presentation)
    ((:number :text-field) 'number-presentation)
    ((:integer nil)        'integer-presentation)
    ((:range nil)          'range-limited-number-presentation)
    ((:date nil)           'date-presentation)
    ((:date-range nil)     'date-range-presentation)
    ((:time nil)           'time-presentation)
    ((:number :measurement) 'measurement-presentation)
    ((:measurement :number) 'measurement-presentation)
    ((:measurement :measurement) 'measurement-presentation)
    ((:choice :dropdown)   'member-select-presentation)
    ((:choice :radio)      'member-radio-presentation)
    ((:number :radio) 'number-radio-presentation)
    ((:multichoice :multiple-dropdown)
                           'multiple-members-select-presentation)))

(defun default-view-for-type (dtype)
   (case dtype
     ((nil) :text-field)
     (:boolean :radio)
     (:string  :text-field)
     (:number  :text-field)
     (:measurement :measurement)
     (:choice      :dropdown)
     (:multichoice :multiple-dropdown)
     (:date    :text-field)
     (:date-range :text-field)
     (:range   :text-field)))

(defmethod subtype-text-value ((data-type (eql :measurement)) subtype)
  "Translate a subtype into the locale appropriate measurement"
  (case subtype
    (:number "")
    (:weight "kg")
    (:length "cm")
    (:bmi "kg/m2")
    (:years "years")
    (:minutes "minutes")
    (:seconds "seconds")))

(defun default-question-value (question)
  "Return the default lisp value for a given question type"
  (case (question-data-type question)
    (:boolean nil)
    (:string "")
    (:number 0)
    (:date (get-universal-time))
    (:date-range (cons (get-universal-time) (get-universal-time)))
    (:time (get-universal-time))
    (:measurement 0)
    (:choice (first (question-choices question)))
    (:multichoice (first (question-choices question)))
    (t nil)))


;;
;; Default format errors
;;

(defparameter *default-errors*
  '((:boolean . "Answer must be yes or no or true and false")
    (:date . "You must provide a valid date (YYYY/MM/DD)")
    (:date-range . "You must provide a valid date range ('YYYY/MM/DD to YYYY/MM/DD')")
    (:string . "You must provide an answer")
    (:number . "You must provide a valid number such as 10, 1.4, etc")
    (:measurement . "Please enter a number in the units specified")
    (:choice . "You must select a choice")
    (:range . "A range consists of two numbers separated by a space and one of '-' 'to' or ':'")))

(defun get-default-error-string (type)
  (awhen (assoc type *default-errors*)
    (cdr it)))

(defmethod format-error-string (question data)
  (declare (ignore data))
  (format nil "~A" (get-default-error-string (question-data-type question))))

