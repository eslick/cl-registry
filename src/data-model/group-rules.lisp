(in-package :registry)

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

;; More rules management utilities

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

(defun activates-inline-group-p (question)
  (awhen (parent question)
    (loop for rule in (group-rules it)
	 when (eq (group-rule-question rule) question)
	 collect (group-rule-target rule))))

(defun find-activating-question (group subgroup)
  (when group
    (loop for rule in (group-rules group)
       when (eq (group-rule-target rule) subgroup) do
       (return (group-rule-question rule))
       finally (return nil))))

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


