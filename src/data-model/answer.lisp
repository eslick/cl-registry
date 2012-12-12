(in-package :registry)

;; ===========================================================
;;  Answer model
;; ===========================================================

(defmodel answer (fulltext-mixin published-data-mixin)
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
	    (if (stringp value)
			(string (subseq value 0 (min (length value) 40)))
			 value))))

(defmethod fulltext-document ((inst answer))
  (when (and (slot-boundp inst 'question)
	     (eq (question-data-type (question inst)) :string))
    (call-next-method)))

(defmethod published-data-p ((instance answer))
  (aif (question instance) (published-data-p it)))

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
		(let ((dups (select-if (f (a) (eq (answer-id a) id)) it)))
		  (cond ((= (length dups) 0) nil)
				((= (length dups) 1) (first dups))
			    (t (prog1 
					   (first dups)
					 (format t "Found from duplicate instances for ~A: ~A~%" user dups)))))
					 ;; (drop-instances (rest dups))))))
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

(defun answers-since (ts)
  (get-instances-by-range 'answer 'entry-time ts (get-universal-time)))

(defun select-answers-since (question user ts)
  (select-if (f (answer) 
               (and (eq (question answer) question)
                    (eq (user answer) user)))
             (answers-since ts)))

(defun cached-answers (question user)
;;  (format t "Getting cached data for ~A ~A~%" (username (user user)) question)
  (when weblocks::*session*
	(let ((cache (weblocks::session-value 'answer-cache))
		  (ts (weblocks::session-value 'answer-cache-ts)))
	  (when (null cache)
		(setf cache (setf (weblocks::session-value 'answer-cache) (make-hash-table)))
        (setf ts (setf (weblocks::session-value 'answer-cache-ts) (get-universal-time))))
	  (aif (gethash question cache)
		   (progn 
;;			 (format t "~A ~A~%" (length it) (length (select-answers-since question user ts)))
			 (union it (select-answers-since question user ts)))
		   (setf (gethash question cache) 
				 (select-if #'(lambda (x)
								(eq (question x) question))
							(get-instances-by-value 'answer 'user user)))))))

(defun get-user-answers (question user)
  "Answers associated with user"
  (if (eq (current-user) (user user))
	  (cached-answers question user)
	  (select-if #'(lambda (x)
					 (eq (question x) question))
				 (get-instances-by-value 'answer 'user user))))

(defun sorted-answers (question user)
  "Return answers sorted on value"
  (sort (get-user-answers question user) #'sort-any->= :key 'value))

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

;; Answer mutation log 

(defvar *answer-log-stream* nil)
(defvar *answer-log-lock* (bordeaux-threads:make-lock))

(define-system-event-hook answer-log-setup (start-app)
  (let ((stream (open (make-pathname :defaults (registry-relative-path (list "logs"))
				     :name "answer-history" :type "log")
		      :direction :output
		      :if-exists :append
		      :if-does-not-exist :create
		      :sharing :lock)))
    (format stream "~%;; Starting answer log ")
    (render-timestamp stream (get-universal-time))
    (format stream "~%~%")
    (force-output stream)
    (setf *answer-log-stream* stream)))

(define-system-event-hook answer-log-shutdown (stop-app)
  (close *answer-log-stream*)
  (setf *answer-log-stream* nil))

(defun log-answer (question patient value id)
  (bordeaux-threads:with-lock-held (*answer-log-lock*)
    (handler-case 
      (if *answer-log-stream*
		  (progn
			(format *answer-log-stream*
					";; ~A entered answer for patient ~A~%;; for q: ~A~%"
					(if (boundp 'hunchentoot::*session*) (current-user) nil)
					patient question)
			(format *answer-log-stream*
					"(answer :question ~A :user ~A :time ~A :value \"~A\" :id ~A )~%"
					(mid question) (mid patient) (get-universal-time) value id))
		  (warn "Answer log not open"))
    (error (e) (warn "Error in answer-log: ~A" e)))))

(defun add-answer (question user value &key id history)
  "Create a new answer"
  (let ((our-history (or history (latest-history-answer question user))))
    (log-answer question user value (or id 1))
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
  (ensure-transaction ()
   (let* ((user (current-patient))
		  (answer (get-answer question user id)))
	 (if answer
		 (update-answer% answer value id)
		 (unless (or (eql value :none) (equal value ""))
		   (add-answer question user value :id id))))))

(defun update-answer% (answer new-value id)
  (cond ((update-history-p answer new-value)
		 (log-answer (question answer) (user answer) new-value (or id 1))
		 (setf (value answer) new-value)
		 (setf (entry-time answer) (get-universal-time))
		 answer)
		((record-history-p answer new-value)
		 (log-answer (question answer) (user answer) new-value (or id 1))
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

(defun delete-diary-entry (survey patient id)
  (let* ((questions (all-survey-questions survey))
		 (answers (remove-nulls (mapcar (f (q) (get-answer q patient id)) questions))))
	answers))
	   
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
  t)
;;  (let ((ids (mapcar #'answer-id (get-user-answers q user))))
;;    (when (neq (length ids) (length (remove-duplicates ids)))
;;      (error "Invalid duplicate answers in question ~A" q))))

(defun remove-duplicate-answers (q user)
  (drop-instances (duplicate-answers q user)))

(defun duplicate-answers (q user)
  (let* ((answers (get-user-answers q user))
		 (unique (when answers (remove-duplicates answers :key #'answer-id))))
    (set-difference answers unique)))

(defun remove-all-duplicates ()
  (map-class #'remove-user-duplicates 'user))

(defun remove-user-duplicates (user)
  (print user) 
  (map-class #'(lambda (question) (remove-duplicate-answers question user)) 'question))

(defun print-user-duplicates (user)
  (print user)
  (map-class #'(lambda (question) 
				 (let ((dups (duplicate-answers question user)))
				   (when dups
					 (print (cons question dups)))))
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

;;
;; Import Answer Log
;;

(defun make-diaries-table ()
  (let ((table (make-hash-table)))
	(populate-question-table table
							 (get-survey "Estrogen Study Diary"))
	(populate-question-table table
							 (get-survey "Dyspnea Diary"))
	table))

(defun populate-question-table (table survey)
  (loop for group in (survey-groups survey)
	 do (loop for subgroup in (find-subgroups group)
		   do (loop for question in (group-questions subgroup)
				 do (setf (gethash (mid question) table) t))))
  table)

(defun make-user-table (users)
  (let ((table (make-hash-table)))
	(loop for user in users 
		 do (setf (gethash (mid (get-patient-for-user user)) table) t))
	table))
		 

(defun non-comment-line-p (line)
  (and (> (length line) 2) (not (equal (subseq line 0 2) ";;"))))

(defun parse-line (line)
  (awhen (read-from-string line)
    (pairs (cdr it))))

(defun matching-rec (rec qtable utable)
  (and (or (null qtable) (gethash (assoc-get :question rec) qtable))
	   (or (null utable) (gethash (assoc-get :user rec) utable))))

(defun update-table (rec table)
  (setf (gethash (list (assoc-get :question rec) 
					   (assoc-get :user rec) 
					   (assoc-get :id rec))
				 table)
		rec))

(defun commit-table (table &optional pmid &aux (count 0))
  (maphash (f (key rec)
              (declare (ignorable key))
              (when (zerop (mod (incf count) 1000))
				(format t "Count: ~A~%" count))
              (when (or (null pmid) (eq pmid (assoc-get :user rec)))
;;                (format t "Evaluating: ~A~%" rec)
				(with-transaction ()
                  (let* ((question (get-question (assoc-get :question rec)))
						 (pat (get-model 'patient (assoc-get :user rec)))
						 (new-value (assoc-get :value rec))
						 (new-value (if (member (question-data-type question) '(:choice))
										(if (numberp new-value) 
											(format nil "~A" new-value)
											new-value)
										new-value))
						 (id (assoc-get :id rec))
						 (answer (get-answer question pat id)))
					(when (null answer) 
					  (format t "Adding (~A): ~A ~A ~A ~A~%" key question pat new-value id)
					  (make-instance 'answer
								   :question question
								   :user pat
								   :value new-value
								   :id id
								   :entry-time (assoc-get :time rec)
								   :history nil))))))
		   table))

(defparameter line-scanner (ppcre:create-scanner "(.* :value) (.*) (:id .*)" :single-line-mode t))

(defun primitive-value-p (string)
  (and (= (length (split-sequence:split-sequence #\Space string)) 1)
	   (or (string= string "NIL")
		   (string= string "\"NIL\"")
		   (string= string "T")
		   (string= string "\"T\"")
		   (parse-integer string :junk-allowed t) ;; number
		   (parse-integer string :start 1 :junk-allowed t) ;; number
		   )))

(defun quoted-p (string)
  (and (eq (char string 0) #\")
	   (eq (last-char string) #\")))

(defun strip-quotes (string)
  (subseq string 1 (- (length string) 1)))

(defun multi-valued-result (string)
  (eq (char string 0) #\())

(defun parse-multiple-values (string)
  (split-sequence:split-sequence #\Space (subseq string 1 (- (length string) 1))))

(defun read-full-line (line)
  (multiple-value-bind (full parts)
	  (scan-to-strings line-scanner line)
	(declare (ignorable full))
	(let ((pre (aref parts 0))
		  (val (aref parts 1))
		  (post (aref parts 2)))
	  (cond ((and (primitive-value-p val) (quoted-p val))
			 (parse-line (format nil "~A ~A ~A" pre (strip-quotes val) post)))
			((and (primitive-value-p val) (not (quoted-p val)))
			 (parse-line (format nil "~A ~A ~A" pre val post)))
			((quoted-p val)
			 (parse-line (format nil "~A ~A ~A" pre val post)))
			((multi-valued-result val)
			 (parse-line (format nil "~A (~{\"~A\" ~}) ~A" pre (parse-multiple-values val) post)))
			(t (parse-line (format nil "~A \"~A\" ~A" pre val post)))))))

(defun handle-line (line table qtable utable)
  (handler-case
	  (let ((rec (read-full-line line)))
		(when (matching-rec rec qtable utable)
;;		  (format t "Adding: ~A~%" rec)
		  (update-table rec table)))
	(error (e)
	  (format t "Error parsing line: ~A~%~A~%" e line))))
			  
(defun update-answers-from-log (file qtable utable)
  (let ((table (make-hash-table :size 10000 :test #'equal))
		(line ""))
	(with-open-file (stream file)
	  (handler-case 
   	     (loop 
		   for segment = (read-line stream nil nil)
		   while segment
		   when (non-comment-line-p segment)
		   do (progn 
;;				(format t "line: ~A~%" line)
				(cond ((or (equal line "") (eq (char line 0) #\())
					   (setf line (format nil "~A~A~%" line segment)))
					  ((eq (char segment 0) #\()
					   (setf line segment))
					  (t (setf line "")))
;;				(format t "Examining: ~A~%" segment)
				(when (eq (last-char segment) (last-char ")"))
				  (handle-line line table qtable utable)
				  (setf line ""))))
	   (error (e) 
		 (format t "Terminating: ~A~%" e)))
	  table)))
;;(commit-table table))))
