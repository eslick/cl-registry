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

(defun drop-group (group)
  (dolist (question (group-questions group))
    (drop-question question))
  (drop-instance group))


;; ===========================================================
;;  Rules capture link structure and behavior
;; ===========================================================

(defstruct group-rule question value target type default)

(defmethod print-object ((rule group-rule) stream)
  (with-struct-slots (group-rule question value target type) rule
    (format stream "#<GROUP-RULE ~A '~A' -> ~A (~A)>" 
	      (object-id question) value (object-id target) type)))

(defun make-rule (question value target type default)
  (make-group-rule :question question :value value :target target 
		   :type type :default default))

(defun make-default-rule (target)
  "Make a default 'fall-through' rule"
  (make-rule nil nil target :inline nil))

(defun match-rule (rule1 rule2)
  "Rules are unique"
  (and (eq (group-rule-question rule1)
	   (group-rule-question rule2))
       (equal (group-rule-value rule1)
	      (group-rule-value rule2))))

(defmethod add-rule ((group survey-group) question value target type &key (default :hide) replace)
  "Insert a rule into a group.  Type can be successor, inline.
   Default can be show/hide.  Rules affect the rendering of questions"
  (let* ((rules (group-rules group))
	 (new-rule (make-rule question value target type default))
	 (match (some #'(lambda (rule) (match-rule new-rule rule)) rules)))
    (remove-empty-rules group)
    (cond ((and match replace)
	   (delete-rule group question value)
	   (setf (group-rules group)
		 (cons new-rule (group-rules group))))
	  (match
	   (return-from add-rule nil))
	  (t (setf (group-rules group) 
		   (cons new-rule (group-rules group)))))
    new-rule))

(defun remove-empty-rules (group)
  "Remove any null rules from the group list"
  (setf (group-rules group)
	(remove-if (lambda (r) (null (group-rule-target r)))
		   (group-rules group))))

(defmethod delete-rule ((group survey-group) question value)
  "Remove a rule from the rule set"
  (let ((rules (group-rules group)))
    (setf (group-rules group)
	  (delete (make-group-rule :question question :value value) rules :test #'match-rule))))

(defun validate-survey-group (group)
  "Ensure that the rules have matching questions"
  (let ((questions (group-questions group))
	(rules (group-rules group)))
    (when (null rules)
      (error "A group must have at least one rule, typically of type 
              :default <group> or :default nil for an end of survey 
              indication"))
    (and 
     ;; The rule set is valid
     (loop 
	for rule in rules
	for question = (find (group-rule-question rule) questions)
	unless (and question (member (group-rule-value rule) (question-choices question)))
	do (return nil)
	finally (return t)))))


;; Some cleaning-up utilities

(defun remove-all-empty-rules ()
  (mapcar #'remove-empty-rules (get-instances-by-class 'survey-group)))

(defun ensure-all-inline-rules ()
  (mapcar (lambda (group)
	    (setf (group-rules group)
		  (mapcar (lambda (rule) 
			    (setf (group-rule-type rule) :inline) 
			    rule)
			  (group-rules group))))
	  (get-instances-by-class 'survey-group)))

(defun clean-up-boolean-rules ()
  (mapcar (lambda (group)
	    (setf (group-rules group)
		  (mapcar (lambda (rule) 
			    (when (eq (question-data-type 
				       (group-rule-question rule))
				      :boolean)
			      (setf (group-rule-value rule) 
				    (let ((value (group-rule-value rule)))
				      (cond ((symbolp value) value)
					    ((equalp value "true") :true)
					    ((equalp value "false") :false)
					    (t (format nil "Unknown boolean value ~A"
						       value)))))
			      (when (stringp (group-rule-value rule)) 
				(print rule))
			      rule))
			  (group-rules group))))
	  (get-instances-by-class 'survey-group)))

(defun cleanup-rule-lists ()
  (mapcar (lambda (group)
	    (setf (group-rules group)
		  (remove-nulls (group-rules group))))
	  (get-instances-by-class 'survey-group)))



;; ===============================================================
;;  Compute the current state of inline groups
;; ===============================================================

(defmethod inline-groups (group question value)
  (let ((rules (select-if (lambda (rule)
			    (eq (group-rule-question rule) question))
			  (group-rules group)))
	(targets nil))
    (dolist (rule rules)
      (when (matching-rule rule question value)
	(push (group-rule-target rule) targets)))
    (append targets 
	    (mapcan #'(lambda (subgroup)
			(inline-groups subgroup question value))
		    targets))))
;;    targets))

(defun matching-rule (rule question value)
  (case (question-data-type question)
    (:boolean (eq (group-rule-value rule) 
		  (case value 
		    ((t) :true)
		    (:none :none)
		    (t :false))))
    (:choice (equal (group-rule-value rule) value))
    (:multichoice (member (group-rule-value rule) value :test 'equal))
    (t (or (and (eq (group-rule-value rule) :any)
		(not (null value))
		(not (equal value "")))
	   (equal (group-rule-value rule) value)))))

;; ===============================================================
;;  Compute active rules based on answers
;; ===============================================================

(defun active-rules (group answers)
  (let ((rules (group-rules group)))
    (loop for rule in rules
         unless (or (default-rule-p rule)
		    (inline-rule-p rule))
	 when (active-rule-p rule answers)
	 collect rule)))

(defun default-rule-p (rule)
  (eq (group-rule-type rule) :default))

(defun inline-rule-p (rule)
  (eq (group-rule-type rule) :inline))

(defun active-rule-p (rule answers)
  (with-struct-slots (group-rule question value) rule
    (when (or (equal value :any)
	      (equal value 
		     (aif-ret 
		      (find-answer-with-question question answers))))
      t)))

(defun has-inline-rule (group question)
  (dolist (rule (group-rules group))
    (when (and (eq (group-rule-question rule) question)
	       (inline-rule-p rule))
      (return-from has-inline-rule t)))
  nil)



;;
;; Little utilities
;;

(defun nsplice-list-after (list inserted element &key (test 'equal))
  (aif (member element list :test test)
       (progn (rplacd it (append inserted (rest it)))
	      list)
       (append inserted list)))

(defun move-list-element (list element insert-after &key (test 'equal))
  (assert (not (some #'null list)))
  (let ((position (if (null insert-after) 0
		      (position insert-after list :test test))))
    (assert (numberp position))
    (ninsert (remove element list) element position)))

(defun list-prev (ref list &aux last-one)
  "Return the element before elt in list"
  (loop for elt in list do
       (if (equal elt ref)
	   (return last-one)
	   (setf last-one elt))))

(defun list-next (ref list)
  "Return the element after elt in list"
  (awhen (cdr (member ref list))
    (car it)))

;; ===============================================================
;;  Manual manipulation of survey groups
;; ===============================================================

(defun get-group (name)
  (assert-body-type 'survey-group
    (typecase name
      (number (get-model 'survey-group name)) ;; by uid
      (survey-group name)	 ;; identity
      (t (get-instance-by-value 'survey-group 'name name)))))

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

(defun make-group (name &key advice questions)
  (make-instance 'survey-group :name name
		 :advice advice
		 :questions questions))

(defun make-inlined-question-group (group question value inlined-questions &key name inline-group)
  (let ((group (get-group group))
	(question (get-question question))
	(inlined-questions (mapcar #'get-question (mklist inlined-questions))))
    (let ((inline-group 
	   (or (and inline-group (get-group inline-group))
	       (make-group (or name 
			       (concatenate 
				'string 
				(princ-to-string (mid question))
				"-" 
				(question-prompt question)))))))
      (move-questions group inlined-questions inline-group)
      (add-rule group question value inline-group :inline)
      inline-group)))

;;(defun make-optional-question-group (group question

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

(defun activates-inline-group-p (question)
  (awhen (parent question)
    (loop for rule in (group-rules it)
	 when (eq (group-rule-question rule) question)
	 collect (group-rule-target rule))))

(defun published-group-p (group)
  (when (member group (published-groups)) t))

(defparameter *published-groups* nil)

(defun published-groups ()
;;  (unless *published-groups*
;    (setf *published-groups*
	  (find-published-groups))
;;  *published-groups*)

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
  (some (curry2 'get-user-answers user)
	(group-questions group)))


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


(defun find-activating-question (group subgroup)
  (when group
    (loop for rule in (group-rules group)
       when (eq (group-rule-target rule) subgroup) do
       (return (group-rule-question rule))
       finally (return nil))))
       

;; ============================================================
;;  DEPRECATED
;; ============================================================

;;
;; Upgrading surveys to linear group model
;;

#+nil
(defun upgrade-to-linear-surveys ()
  (dolist (survey (get-instances-by-class 'survey))
    (unless (survey-groups survey)
      (setf (survey-groups survey)
	    (get-top-level-groups survey)))
    (dolist (group (survey-groups survey))
      (upgrade-group group))))

#+nil
(defun get-top-level-groups (survey)
  (let ((root (first (survey-groups survey))))
    (when root (find-top-level-groups root))))

#+nil
(defun find-top-level-groups (root &aux groups)
  (loop 
     for group = root then (group-rule-target 
			    (first
			     (select-if #'default-rule-p (group-rules group))))
     while group do
       (push group groups)
       (upgrade-group group))
  (nreverse groups))

#+nil
(defun upgrade-group (group)
  (loop for question in (group-questions group) do
       (setf (parent question) group)))

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