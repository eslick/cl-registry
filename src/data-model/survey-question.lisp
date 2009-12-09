(in-package :registry)

(registry-proclamations)

;; ===========================================================
;;  Survey question model
;; ===========================================================

;; Prompt, type, view, relation
;; => validation, extraction

(defmodel question (fulltext-mixin user-translation-mixin)
  (;; Prompt
   (name :accessor question-name :initarg :name)
   (prompt :accessor question-prompt :initarg :prompt :index t) ;; statisfy via fulltext!
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
   (required :accessor question-required-p :initarg :required :initform nil))
  (:documentation "The base class for survey and diary questions."))

(defmethod fulltext-fields ((instance question))
  '(name prompt question-help))

(defmethod print-object ((inst question) stream)
  (format stream "#<QUESTION (~A) '~A'>"
	  (object-id inst)
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

(defmethod drop-instance ((inst question))
  (when (drop-question inst)
    (call-next-method)))

(defun drop-question (question &optional (interactive t))
  (when (or (not interactive)
	    (let ((answers (length (get-instance-by-value 'answer 'question question))))
	      (if (eq answers 0) t
		  (when (yes-or-no-p "Question ~A has ~A associated answers, do you really want to delete this question?" question answers)
		    (when (yes-or-no-p "Are you really really sure?")
		      t)))))
    (mapcar #'drop-instance (get-instances-by-value 'answer 'question question))
    t))


;; Protocol for rendering in generic views

(defmethod results-title ((inst question))
  (question-prompt inst))

;; Search question structure

(defun parent-group (question)
  (cond ((equal (type-of question) 'question)
	 (parent-group (parent question)))
	(t nil)))

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

(defun related-questions-in-context (question)
  (let ((related nil))
    (awhen (parent question)
      (setf related (set-difference (group-questions it) (list question))))
    (when (< (length related) 10)
      (setf related 
	    (append related 
		    (filter-if (lambda (obj)
				 (not (eq (type-of obj) 'question)))
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
	(args `((:prompt ,(slot-value-translation question 'prompt)))))
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
		   args))))
    (let ((presentation 
	   (apply #'make-instance type 
		  :metadata question 
		  :css-class "question-presentation"
		  (flatten1 (cons initargs args)))))
      (when (eq type 'number-presentation)
	(push (make-instance 'number-validator)
	      (validators presentation)))
      (setf (dom-id presentation) (query-name presentation))
      (aif (get-answer question (current-user) id)
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
    (:time (get-universal-time))
    (:measurement 0)
    (:choice (first (question-choices question)))
    (:multichoice (first (question-choices question)))
    (t nil)))



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

  
;; ===========================================================
;;  Answer model
;; ===========================================================

(defmodel answer (fulltext-mixin)
  ((question :accessor question :initarg :question :index t)
   (user :accessor user :initarg :user :index t)
   (entry-time :accessor entry-time :initarg :entry-time :index t)
   (value :accessor value :initarg :value :initform nil)
   (id :accessor answer-id :initarg :id :initform 1)
   (history :accessor answer-history :initarg :history :initform nil)))

(defmethod fulltext-fields ((instance answer))
  '(value))

(defmethod print-object ((inst answer) stream)
  (format stream "#<ANSWER (~A.~A) '~A'>" 
	  (object-id inst) (answer-id inst)
	  (let ((value (value inst)))
	    (typecase value
	      (string (subseq value 0 (min (length value) 40)))
	      (t value)))))

(defmethod fulltext-document ((inst answer))
  (when (and (slot-boundp inst 'question)
	     (eq (question-data-type (question inst)) :string))
    (call-next-method)))

(defpclass answer-history (answer)
  ((question :accessor question :initarg :question :index t :inherit nil)
   (user :accessor user :initarg :user :index t :inherit nil)
   (entry-time :accessor entry-time :initarg :entry-time :index t :inherit nil)))


(defmethod print-object ((inst answer-history) stream)
  (format stream "#<ANS-HIST (~A.~A) '~A'>" 
	  (object-id inst) (answer-id inst)
	  (let ((value (value inst)))
	    (typecase value
	      (string (subseq value 0 (min (length value) 40)))
	      (t value)))))

;; ===================================
;;  Accessors and Search
;; ===================================

(defun find-answer-with-question (question answers)
  "Can ignore database here as answer sets are small"
  (find question answers :key #'question))

(defun get-answer (question user &optional id)
  "Get the answer to question for user; optionally use ID"
  (awhen (get-user-answers question user)
    (if id 
	(find id it :key #'answer-id)
    (if (diary-question-p question)
	(most #'answer-id it)
	(first it)))))

(defun latest-answer (question user)
  "Use 'most' to get the latest diary answer by entry time"
  (most #'entry-time (get-user-answers question user)))

(defun get-answers (question)
  "Answers associated with question"
  (let ((answers (get-instances-by-value 'answer 'question question)))
    answers))

(defun has-answer-p (question)
  "Return the first answer"
  (get-instance-by-value 'answer 'question question))

(defun get-user-answers (question user)
  "Answers associated with user"
  (select-if #'(lambda (x)
		 (eq (question x) question))
	     (get-instances-by-value 'answer 'user user)))

(defun sorted-answers (question user)
  "Return answers sorted on value"
  (sort (get-user-answers question user) #'> :key 'value))

(defun latest-history-answer (question user)
  "Lookup any history objects and given the last one recorded"
  (first (sort 
	  (intersection 
	   (get-instances-by-value 'answer-history 'question question)
	   (get-instances-by-value 'answer-history 'user user))
	  #'>
	  :key 'entry-time)))

(defun validate-answers (q user)
  (let ((ids (mapcar #'answer-id (get-user-answers q user))))
    (when (neq (length ids) (length (remove-duplicates ids)))
      (error "Invalid duplicate answers in question ~A" q))))

(defun remove-duplicate-answers (q user)
  (drop-instances (duplicate-answers q user)))

(defun duplicate-answers (q user)
  (let ((latest (latest-answer q user)))
    (set-difference (get-user-answers q user) (list latest))))

(defun remove-all-duplicates ()
  (map-class #'remove-user-duplicates 'user))

(defun remove-user-duplicates (user)
  (print user) 
  (map-class #'(lambda (question) (remove-duplicate-answers question user)) 'question))

(defun print-user-duplicates (user)
  (print user)
  (map-class #'(lambda (question) (let ((dups (duplicate-answers question user)))
				    (when dups
				      (print (cons (latest-answer question user) dups)))))
	      'question))

;; ==================================
;;  Mutators
;; ==================================

(defun add-answer (question user value &key id history)
  "Create a new answer"
  (let ((our-history (or history (latest-history-answer question user))))
    (make-instance 'answer 
		   :question question
		   :user user
		   :entry-time (get-universal-time)
		   :value value
		   :id (or id 1)
		   :history our-history)))

(defun next-id (question user)
  (let ((answers (sort (get-user-answers question user) #'> :key 'answer-id)))
    (if answers
	(1+ (answer-id (first answers)))
	1)))

(defun update-answer (question value &optional id)
  "Updates the answer; or latest answer"
  (let* ((user (current-user))
	 (answer (get-answer question user id)))
    (if answer
	(update-answer% answer value id)
	(unless (or (eql value :none) (equal value ""))
	  (add-answer question user value :id id)))))

(defun update-answer% (answer new-value id)
  (cond ((update-history-p answer new-value)
	 (setf (value answer) new-value)
	 (setf (entry-time answer) (get-universal-time))
	 answer)
	((record-history-p answer new-value)
	 (add-answer (question answer) (user answer) new-value :id id
		     :history (change-class answer 'answer-history)))
	(t answer)))

(defun record-history-p (answer value)
  (not (equalp value (value answer))))

(defparameter *provenance-recording-timeout* #.(* 60 60 24)
	      "When one day has passed, record the prior value for an answer
               if it has changed")

(defun update-history-p (answer value)
  (and (record-history-p answer value)
       (< (get-universal-time) (+ (entry-time answer)
				  *provenance-recording-timeout*))))

(defun delete-answers (questions)
  (mapc #'delete-answer questions))

(defun delete-answer (question &optional user)
  (awhen (get-answer question (or user (current-user)))
    (cond ((update-history-p it :delete)
	   (drop-instances (mklist it)))
	  ((record-history-p it :delete)
	   (mapc #'(lambda (answer)
		     (change-class answer 'answer-history))
		 (mklist it))))))
	   



;; ===================================
;;  Convenience functions
;; ===================================

(defun get-object (id)
  (elephant::controller-recreate-instance *store-controller* id))

(defun get-user (username)
  (cond ((subtypep (type-of username) 'user)
	 username)
	((numberp username)
	 (get-model 'user username))
	(t (get-instance-by-value 'user 'username username))))

(defun answers-by-data-type (data-type)
  (collect (lambda (a)
	     (when (eq (question-data-type (question a)) data-type)
	       a))
    (get-instances-by-class 'answer)))

(defmacro assert-body-type (type &body body)
  (with-gensyms (value)
    `(let ((,value (progn ,@body)))
       (assert (eq (type-of ,value) ,type))
       ,value)))

(defun get-question (question-ref)
  (assert-body-type 'question
    (typecase question-ref
      (number (get-model 'question question-ref))
      (question question-ref)
      (t (get-instance-by-value 'question 'prompt question-ref)))))

(defun answers-by-question (question-prompt &optional user)
  (let* ((question (get-question question-prompt))
	 (answers  (get-instances-by-value 'answer 'question question))
	 (answer (when user (find user answers :key #'user))))
    (if user answer answers)))


;; ===================================
;;  Utilities
;; ===================================

(defun upgrade-question-answers (question fn)
  "Updates answers with new value returned by function when
   second return value is true"
  (labels ((change-answer (answer value)
	     (update-answer% answer value (answer-id answer)))
	   (upgrade-answer (answer)
	     (multiple-value-bind (value change?)
		 (funcall fn (value answer))
	       (if change?
		   (change-answer answer value)
		   answer))))
    (mapcar #'upgrade-answer
	    (get-answers question))))

(defun list-replace (old new list &key (test #'equal))
  "Cons a new list, replacing instance of old with new under equal"
  (print test)
  (cond ((null list) nil)
	((funcall test old (car list))
	 (cons new #1=(list-replace old new (cdr list) :test test)))
	(t (cons (car list) #1#))))

(defun upgrade-multichoice-value-fn (old-value new-value)
  (lambda (list)
    (if (find old-value list :test #'equal)
	(values (list-replace old-value new-value list) t)
	(values list nil))))

(defun upgrade-choice-value-fn (old-value new-value)
  (lambda (value)
    (if (equal old-value value)
	(values new-value t)
	(values value nil))))

#+clsql
(defun convert-date (date)
  (multiple-value-bind (y m d) (clsql-sys::date-ymd date)
    (multiple-value-bind (a b ut) 
	(parse-datetime (format nil "~a/~a/~a" y m d))
      (declare (ignore a b))
      ut)))

(defun convert-boolean-to-tag (answers)
  (mapcar #'(lambda (answer)
	      (let ((question (question answer)))
		(when (and (eq (question-data-type question) :boolean)
			   (not (member (value answer) '(:true :false))))
		  (setf (value answer) 
			(if (value answer) :true :false)))))
	  answers))

(defun convert-all-boolean-rules-to-tag (groups)
  (mapc #'convert-boolean-rule-to-tag groups))

(defun convert-boolean-rule-to-tag (group)
  (let ((rules (group-rules group)))
    (dolist (rule rules)
      (awhen (group-rule-question rule)
	(when (eq (question-data-type it) :boolean)
	  (setf (group-rule-value rule)
		(if (group-rule-value rule) :true :false)))))
    (setf (group-rules group) rules)))

(defun remove-default-rules (group)
  (let ((rules (group-rules group))
	(new-rules nil))
    (dolist (rule rules)
      (when (group-rule-question rule)
	(push rule new-rules)))
    (setf (group-rules group) new-rules)))

(defun ensure-unique-question-names (questions)
  (let* ((count 0))
    (mapc #'(lambda (q)
	      (handler-case 
		  (progn
		    (when (zerop (mod (incf count) 10))
		      (format t "Converted ~A questions~%" count))
		    (let ((name (question-name q)))
		      (loop for testq in (nthcdr (1- count) questions) do
			   (when (equal name (question-name testq))
			     (setf (question-name testq)
				   (concatenate 'string (question-name testq) 
						(format nil "~A" count)))))))
		(error (e)
		  (cerror (format nil "Keep processing") "Found error ~A" e))))
	  questions)))

(defun map-questions-by-type (fn type)
  (map-class (lambda (question)
	       (when (eq (question-data-type question) type)
		 (funcall fn question)))
	     'question))

(defun map-answers-for-question (fn question)
  (loop for answer in (get-instances-by-value 'answer 'question question) do
       (funcall fn answer)))

  
;; ===========================================================
;;  Complex data type support
;; ===========================================================

;;
;; Printing
;;

;; (defun client-text-value (type lisp-value)
;;   (case type
;;     (:boolean (if lisp-value "yes" "no"))
;;     (:number (if (not lisp-value) nil
;; 		 (typecase lisp-value
;; 		   ((or float double-float)
;; 		    (format nil "~,1f" lisp-value))
;; 		   (number (format nil "~d" lisp-value)))))
;;     (:measurement (if (not lisp-value) nil
;; 		      (typecase lisp-value
;; 			((or float double-float)
;; 			 (format nil "~,1f" lisp-value))
;; 			(number (format nil "~d" lisp-value)))))
;;     (:string (aif-ret lisp-value ""))
;;     (:date   (typecase lisp-value
;; 	       (number (with-output-to-string (stream) 
;; 			 (cl-l10n:print-time lisp-value
;; 					     :show-date t 
;; 					     :show-time nil 
;; 					     :stream stream)))
;; 	       (string lisp-value)
;; 	       (null "")))
;;     (:range (when lisp-value (format nil "~A - ~A" (car lisp-value) (cdr lisp-value))))
;;     (:comma-list (when lisp-value (format nil "~{~A~^ ~}" lisp-value)))
;;     (:newline-list (when lisp-value (format nil "~{~A~^~%~}" lisp-value)))))


;;
;; Parsing
;;

;; (defun parse-response (question string)
;;   "Take the string response from the client and compute
;;    a lisp value based on the question"
;;   (parse-text-value (question-data-type question) string))

;; (defun parse-text-value (type client-value)
;;   (case type
;;     (:boolean (if (or (equal client-value "yes")
;; 		      (equal client-value "true")
;; 		      (equalp client-value "t"))
;; 		  :true
;; 		  :false))
;;     (:number (read-from-string client-value))
;;     (:measurement (read-from-string client-value))
;;     (:string client-value)
;;     (:date   (cl-l10n:parse-time client-value))
;;     (:range (parse-range client-value))
;;     (:choice client-value)
;;     (:multichoice client-value)))

;; (defun parse-range (string)
;;   (multiple-value-bind (first chars)
;;       (read-from-string string t :eof)
;;     (when (and first (numberp first))
;;       (multiple-value-bind (separator chars)
;; 	  (read-from-string string t :eof :start chars)
;; 	(when (member separator '(|-| to |:|) :test #'equalp)
;; 	  (let ((second (read-from-string string t :eof :start chars)))
;; 	    (when (and second (numberp second))
;; 	      (cons first second))))))))

;;
;; Validation
;;


;; (defun validate-answer (type data requiredp)
;; ;;  (unless (and requiredp (not data))
;; ;;    (return-from validate-answer nil)
;;   (handler-case 
;;       (ecase type
;; 	(:boolean (or (eq data :true) (eq data :false)))
;; 	(:number (numberp data))
;; 	(:measurement (numberp data))
;; 	(:string (and (stringp data)
;; 		      (if requiredp (> (length data) 0) t)))
;; 	(:date   (numberp data))
;; 	(:range  (< (car data) (cdr data)))
;; 	(:choice (and (consp data) (stringp (first data))))
;; 	(:multichoice (and (consp data) (stringp (first data)))))
;;     (error (e)
;;       (log-message :survey :info "Validation error for ~A ~A; ~A" type data e)
;;       nil)))

;;
;; Default format errors
;;

(defparameter *default-errors*
  '((:boolean . "Answer must be yes or no or true and false")
    (:date . "You must provide a valid date (MM/DD/YY or DD/MM/YY)")
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

