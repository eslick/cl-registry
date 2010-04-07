(in-package :registry)

(registry-proclamations)

;; ===========================================================
;;  Branching Survey Support
;; ===========================================================

(defmodel survey-group (user-translation-mixin)
  ((name :accessor group-name :initarg :name :initform "" :index t)
   (advice :accessor group-advice :initarg :advice :initform "")
   (questions :accessor group-questions :initarg :order :initform nil)
   (rules :accessor group-rules :initarg :rules :initform nil)
   (owner :accessor owner :initarg :owner :initform nil)
   (acl :accessor group-acl :initarg :acl :initform nil)
   (parent :accessor parent :initarg :parent :initform nil) ;; inlined groups only
   (type :accessor group-type :initarg :group-type :initform :single))
  (:documentation "A survey is a set of groups which may contain one or more questions.  Each question is related to an answer. Groups are organized using rules.  The default merely points to the next group.  Question ordering is determined by a field on questions (this sucks, better approach?)"))

(defmethod print-object ((inst survey-group) stream)
  (format stream "#<GROUP-~A '~A'>"
	  (object-id inst)
	  (group-name inst)))

(defmethod translate-fields ((obj survey-group))
  '(name advice))

(defun diary-group-p (group)
  (some 'diary-p (find-group-surveys group)))

(defmethod humanize-name ((inst survey-group))
  (humanize-name (group-name inst)))

(defmethod attributize-name ((inst survey-group))
  (attributize-name (group-name inst)))

(defun get-group (name)
  (assert-body-type 'survey-group
    (typecase name
      (number (get-model 'survey-group name)) ;; by uid
      (survey-group name)	 ;; identity
      (t (get-instance-by-value 'survey-group 'name name)))))

(defun drop-group (group)
  (dolist (question (group-questions group))
    (drop-question question))
  (drop-instance group))

(defun make-group (name &key advice questions)
  (make-instance 'survey-group :name name
		 :advice advice
		 :questions questions))

;; ===============================================================
;;  Manual manipulation of survey groups
;; ===============================================================

(defun move-questions (main-group questions target-group &optional (insert-after nil insert-after-p))
  (let ((questions (mapcar #'get-question (mklist questions)))
	(main-group (get-group main-group))
	(target-group (get-group target-group))
	(insert-after (when insert-after (get-question insert-after))))
    ;; Do lots of checks first!
    (assert (eq (type-of main-group) 'survey-group))
    (assert (eq (type-of target-group) 'survey-group))
    (assert (every (lambda (question)
		     (member question (group-questions main-group)))
		   questions))
    (assert (or (null insert-after)
		(eq (type-of insert-after) 'question)))
    ;; Copy questions to target
    (mapc #'(lambda (q)
	      (setf (parent q) target-group))
	  questions)
    (setf (group-questions target-group)
	  (if insert-after-p
	      (nsplice-list-after (group-questions target-group)
				 questions
				 insert-after)
	      (append (group-questions target-group)
		      questions)))
    ;; Remove from source
    (setf (group-questions main-group)
	  (filter-if (lambda (question)
		       (member question questions))
		     (group-questions main-group)))
    ;; Migrate rules
    (mapc #'(lambda (question)
	      (let ((rules (select-if #'(lambda (r)
					  (eql (group-rule-question r)
					       question))
				      (group-rules main-group))))
		(setf (group-rules main-group)
		      (set-difference (group-rules main-group) rules
				      :key #'group-rule-question))
		(setf (group-rules target-group)
		      (append (group-rules target-group) rules))))
	  questions)))

(defun move-group-question (group question insert-after)
  (let ((group (get-group group))
	(question (get-question question))
	(insert-after (get-question insert-after)))
    (setf (group-questions group)
	  (move-list-element (group-questions group) question insert-after))))

(defun delete-question (group question)
  "Can add conditions here to work with action handlers for the survey editor"
  (when (get-answers question)
    (error "This question has associated answers and can only be deleted
            when the answers have also been deleted"))
  (when (yes-or-no-p "Are you sure you want to delete question ~A from ~A" question group)
    (setf (group-questions group)
	  (remove question (group-questions group)))
    (drop-instance question)))

(defun remove-question (group question)
  (let ((group (get-group group)))
    (setf (group-questions group)
	  (remove (get-question question) (group-questions group)))))

(defun set-question-prompt (question prompt)
  (let ((question (get-question question)))
    (if question
	(progn (setf (question-prompt question) prompt)
	       question)
	:no-question)))

(defun set-question-view (question type)
  (let ((question (get-question question)))
    (if question
	(progn (setf (question-view-type question) type)
	       question)
	:no-question)))

;;
;; Traversing groups and surveys
;;

(defun published-group-p (group)
  (when (member group (published-groups)) t))

(defparameter *published-groups* nil)

(defun published-groups ()
  (find-published-groups))

(defun find-published-groups ()
  (mapcan #'find-subgroups (top-level-groups t)))

(defun top-level-groups (&optional published-only)
  (remove-duplicates 
   (flatten (mapcar #'survey-groups 
		    (if published-only (published-surveys)
			(get-instances-by-class 'survey))))))
		      


(defun find-subgroups (group)
  (cons group 
	(loop for rule in (group-rules group)
	   for group = (group-rule-target rule) appending
	     (when group (find-subgroups group)))))

(defun walk-subgroups (fn group)
  (loop for rule in (group-rules group)
     for group = (group-rule-target rule) do
     (funcall fn group)))

(defun find-group-surveys (group)
  (select-if (lambda (survey)
	       (some (lambda (sgrp)
		       (or (eq sgrp group)
			   (member group (find-subgroups sgrp))))
		     (survey-groups survey)))
	     (published-surveys)))

(defun find-all-group-surveys (group)
  (select-if (lambda (survey)
	       (some (lambda (sgrp)
		       (or (eq sgrp group)
			   (member group (find-subgroups sgrp))))
		     (survey-groups survey)))
	     (get-instances-by-class 'survey)))

(defun group-answered-p (user group)
  (let ((questions (group-questions group)))
    (or (null questions)
	(some (curry2 'get-user-answers user) questions))))

;;
;; Group linking
;;

(defun install-group-parent-links ()
  "Link all inline groups to their parents"
  (loop for group in (top-level-groups) do
       (progn
	 (install-question-parent-links group)
	 (walk-subgroups (link-parent-pointers group) group))))

(defun link-parent-pointers (parent)
  "Generate a function that when called with a group 
   sets its parent pointer to parent, and recurses"
  (lambda (group)
    (setf (parent group) parent)
    (install-question-parent-links group)
    (walk-subgroups (link-parent-pointers group) group)))

(defun install-question-parent-links (group)
  (mapcar #'(lambda (question)
	      (setf (parent question) group))
	  (group-questions group)))


;;
;; Views for survey-group instances
;;

(defview survey-group-simple-view (:type table)
;;  elephant::oid
  name
  advice
  (owner :reader (lambda (group)
			(aif (owner group)
			     (username it)
			     "None"))))

(defview survey-group-table-view (:type table)
  elephant::oid
  name)

(defview survey-group-form-view (:type form)
  name
  (owner :present-as (dropdown :choices #'(lambda (x) 
					    (declare (ignore x))
					    (all-users))
			       :label-key #'username)
	 :parse-as (mid)
	 :reader (compose #'mid #'owner)))

(defview survey-group-data-view (:type data)
  name
  owner)

(defview survey-group-new-view (:type form)
  name
  advice)


;; ===========================================================
;;  Branching Survey Support
;; ===========================================================

(defmodel survey-group-table (survey-group)
  ((rows :accessor group-table-rows :initform nil :initarg :rows))
  (:documentation "A survey-group-table is a compact representation for groups whose answers are all of teh same 'type'"))

(defmethod print-object ((inst survey-group-table) stream)
  (format stream "#<GROUP-TABLE-~A '~A'>"
	  (object-id inst)
	  (group-name inst)))

(defmacro make-survey-group-table ((&rest make-instance-args &key name default-question-args &allow-other-keys) &rest rows)
  (let ((question-counter 0)
        (group-name name)
        (question-bindings '())
        (table '()))
    (remf make-instance-args :default-question-args)
    (setf table
          (list* 'list
                (mapcar (lambda (row)
                          (list* 'list
                                (mapcar (lambda (cell-data)
                                          (cond
                                            ((stringp cell-data) `',cell-data)
                                            ((eql :question (first cell-data))
                                             (destructuring-bind (&rest args
                                                                        &key (name (incf question-counter))
                                                                        &allow-other-keys)
                                                 (cdr cell-data)
                                               (let* ((uname (format nil "~A.~D" group-name name))
						      (name (or (getf args ':name) uname))
                                                      (var (gensym uname)))
						 (remf args :name)
                                                 (push (list var `(make-question ,name ,@args ,@default-question-args)) question-bindings)
                                                 var)))))
                                        row)))
                        rows)))
    `(let ,question-bindings
       (make-instance 'survey-group-table
                      ,@make-instance-args
                      :order (list ,@(mapcar #'first question-bindings))
                      :rows ,table))))

;; ===========================================================
;; Fix broken survey group advice 
;; Issue #201 Render group advice and remove bogus advice text
;; ===========================================================

(defun fix-broken-survey-group-advice (&key (test t) (verbose t))
  (if verbose
      (format t "~%~:[Fix~;Test~] broken survey group advice" test))
  (with-transaction ()
    (let ((null-counter 0.)
	  (not-stringp-counter 0.)
	  (empty-counter 0.)
	  (test-counter 0.)
	  (ok-counter 0.)
	  (fix-counter 0.)
	  (total-counter 0.))
      ;; For each group
      (dolist (group (get-instances-by-class 'survey-group))
	(incf total-counter)
	(let ((advice (group-advice group))
	      fix-p)
	  (cond
	    ((null advice)
	     (incf null-counter))
	    ((not (stringp advice))
	     (if verbose
		 (format t "~&!!  ~@[Survey: ~A ~]Advice: ~S" (first (find-group-surveys group)) advice))
	     (incf not-stringp-counter))
	    ((zerop (length (remove #\space advice)))
	     (incf empty-counter))
	    ((member advice '("test" "testing") :test #'string-equal)
	     (incf test-counter)
	     (setq fix-p t))
	    (t
	     (incf ok-counter)
	     (if verbose
		 (format t "~&Ok: ~S ..." (subseq advice 0. (min 20. (length advice)))))))
	  ;; Fix broken advice?
	  (when fix-p
	    (incf fix-counter)
	     (if verbose
		 (format t "~&~:[Fix~;Test~]: ~@[Survey: ~A ~]Advice: ~S" test (first (find-group-surveys group)) advice))
	     (if (not test)
		 (setf (group-advice group) nil)))))
      ;; Done
      (if verbose
	  (format t
		  "~%Total: ~D~& Null: ~D Not string: ~D Empty: ~D Test: ~D~& Ok: ~D~& Fix: ~D"
		  total-counter null-counter not-stringp-counter empty-counter test-counter ok-counter fix-counter))
      ;; Returns
      (values fix-counter ok-counter total-counter))))
