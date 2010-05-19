(in-package :registry)

;; ============================================================================
;;  Rules capture survey order, dependencies, and display behavior within study
;; ============================================================================

(defconstant +survey-rule-types+ '(:OPTIONAL :REQUIRED :DOFIRST))

(defstruct survey-rule
  survey				;survey object
  type					;survey rule type, as above
  url					;link to external survey (optional)
  )

(defmethod print-object ((rule survey-rule) stream)
  (with-struct-slots (survey-rule survey group question type) rule
    (format stream "#<SURVEY-RULE ~@[~A ~]](~:[default~;~:*~A~])>"
	    (and survey (object-id survey))
	    type)))

(defun match-survey-rule (rule1 rule2)
  "Survey rules are EQUAL if they refer to the same survey"
  (eq (survey-rule-survey rule1)
      (survey-rule-survey rule2)))

;; TBD: adjust generic method signature ADD-RULE to reconcile with method for GROUP-RULES class

(defmethod add-survey-rule ((study study) survey &key (rule-type ':optional) url replace)
  "Insert a survey rule into a study. Type must be one of +SURVEY-RULE-TYPES+
  If REPLACE is non-nil, the new rule can replace an existing rule matching per MATCH-SURVEY-RULE."
  ;; Validate rule type as null or keyword symbol
  (unless (or (null rule-type) (member (setq rule-type (or (as-keyword rule-type) rule-type)) +survey-rule-types+))
    (error "Rule type value ~A is not one of ~S" rule-type +survey-rule-types+))
  (let* ((rules (survey-rules study))
	 (this-rule-type (or rule-type (first +survey-rule-types+)))
	 (new-rule (make-survey-rule :survey survey :type this-rule-type :url url))
	 (match (some #'(lambda (rule) (match-survey-rule new-rule rule)) rules)))
    (remove-empty-survey-rules study)
    (cond
      ((and match replace)
       (delete-survey-rule study survey)
       (setf (survey-rules study)
	     (cons new-rule (survey-rules study))))
      (match
	  (return-from add-survey-rule nil))
      (t
       (setf (survey-rules study)
	     (cons new-rule (survey-rules study)))))
    ;; Returns
    new-rule))

(defun remove-empty-survey-rules (study)
  "Remove any null rules from the survey rules list"
  (setf (survey-rules study)
	(remove-if (lambda (r) (null (survey-rule-survey r)))
		   (survey-rules study))))

(defun delete-survey-rule (study survey)
  "Delete a rule from the survey rules list"
  (check-type study study)
  (let ((rules (survey-rules study)))
    (setf (survey-rules study)
	  ;; This works but it conses a new rule:
	  ;;   (delete (make-survey-rule :survey survey) rules :test #'match-survey-rule)
	  (loop for rule in rules
	       when (not (eq survey (survey-rule-survey rule)))
	       collect rule))))

(defun collect-study-surveys-and-rules (study rules)
  "Replace surveys and survey rules for STUDY from elements in RULES"
  (loop for spec in rules
     with surveys 
     with rules
     do (let ((survey (first spec))
	      (rule-type (second spec))
	      (url (third spec)))
	  (push survey surveys)
	  ;; Here we could use ADD-SURVEY-RULE but this is faster
	  (push (make-survey-rule :survey survey :type rule-type :url url) rules))
     finally
     (setf (surveys study) (reverse surveys) (survey-rules study) rules)))
