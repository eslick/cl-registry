(in-package :registry)

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
;; Update from old lamsight database
;; ===================================

(defun update-answers-with-user-patients ()
  "Update all the ANSWER objects so that the USER slot contains a PATIENT,
   not a USER, instance."
  (let ((oids (map-class 'identity 'answer :collect t :oids t))
        (transaction-size 200)
        (changed-count 0))
    (loop
       while oids
       do
         (with-transaction ()
           (loop
              for count from 0 below transaction-size
              while oids
              for oid = (pop oids)
              for answer = (get-object oid)
              for user = (user answer)
              when (typep user 'user)
              do
                (setf (user answer) (get-patient-home-patient user))
                (incf changed-count))))
    changed-count))

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

(defun get-answers (question)
  "Answers associated with question"
  (let ((answers (get-instances-by-value 'answer 'question question)))
    answers))

(defun map-answers-for-question (fn question)
  (loop for answer in (get-answers question) do
       (funcall fn answer)))

(defun has-answer-p (question)
  "Return the first answer"
  (get-instance-by-value 'answer 'question question))

(defun latest-answer (question user)
  "Use 'most' to get the latest diary answer by entry time"
  (most #'entry-time (get-user-answers question user)))

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
  (let* ((user (current-patient))
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
  (awhen (get-answer question (or user (current-patient)))
    (cond ((update-history-p it :delete)
	   (drop-instances (mklist it)))
	  ((record-history-p it :delete)
	   (mapc #'(lambda (answer)
		     (change-class answer 'answer-history))
		 (mklist it))))))
	   
;; ===================================
;;  Convenience functions
;; ===================================

(defun answers-by-data-type (data-type)
  (collect (lambda (a)
	     (when (eq (question-data-type (question a)) data-type)
	       a))
    (get-instances-by-class 'answer)))

(defun answers-by-question (question-prompt &optional user)
  (let* ((question (get-question question-prompt))
	 (answers  (get-instances-by-value 'answer 'question question))
	 (answer (when user (find user answers :key #'user))))
    (if user answer answers)))


;; ===================================
;; Handle duplicate answers
;; ===================================

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


