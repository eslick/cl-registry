(in-package :registry)

;; ==============================================
;;  Manual Import
;; ==============================================

(defun import-survey-records (records)
  (let ((hash (make-hash-table)))
    ;; Process all records (groups before surveys)
    (dolist (record records)
      (destructuring-bind (id type &rest args) record
	(declare (ignore args))
	(when (gethash id hash)
	  (error "Duplicate ID ~A found in import" id))
	(import-object type record hash)))
    ;; Return the surveys
    (collect (lambda (object)
	       (if (subtypep (type-of object) 'survey)
		   object nil))
      (hash-values hash))))

(defun import-survey-record-file (filename)
  (let ((records nil))
    (with-open-file (stream filename)
      (handler-case
	  (loop for data = (read stream) do
	       (push data records))
	(end-of-file ()
	  nil)))
    (import-survey-records records)))

(defun get-imported-object (id hash &optional (error-p t))
  (aif-ret (gethash id hash)
	   (when error-p (error "Referenced id ~A not yet read" id))))

(defun register-imported-object (obj id hash)
  (if (gethash id hash)
      (warn "Trying to register object ~A as ~A when id already exists" obj id )
      (setf (gethash id hash) obj)))

(defmethod import-object ((type (eql :survey)) expr hash)
  "(id :survey name :description ... :help .. :root ... :owner ... :acl ...))"
  (when (member (third expr) (get-instances-by-class 'survey))
    (warn "Survey '~A' already exists; ignoring" (third expr)))
  (destructuring-bind (id survey name &key description help root owner) expr
    (declare (ignore survey))
    (register-imported-object 
     (make-instance 'survey 
		    :name name
		    :description description
		    :help help
		    :root (get-imported-object root hash)
		    :owner (when owner (get-instance-by-value 'user 'username owner))
		    :acl nil)
	    id hash)))

(defmethod import-object ((type (eql :group)) expr hash)
  "(id :group name question-order rules)"
  (when (member (third expr) (get-instances-by-class 'survey-group))
    (warn "Group '~A' already exists; ignoring" (third expr)))
  (destructuring-bind (id group name order rules) expr
    (declare (ignore group))
    (register-imported-object
     (make-instance 'survey-group
		    :name name
		    :order (mapcar #'(lambda (q) (import-object :question q hash)) order)
		    :rules (mapcar #'(lambda (r) (import-object :group-rule r hash)) rules))
     id hash)))

(defmethod import-object ((type (eql :group-rule)) expr hash)
  "(:rule question-id value target-id type &optional default)"
  (assert (eq (first expr) :group-rule))
  (destructuring-bind (rule question-id value target-id type &optional default) expr
    (declare (ignore rule))
    (make-rule (when question-id (get-imported-object question-id hash))
	       value 
	       (when target-id (get-imported-object target-id hash nil))
	       type
	       default)))

(defmethod import-object ((type (eql :question)) expr hash)
  "(id :question name prompt :help :type :choices :view :relations)"
;;  (when (member (third expr) (get-instances-by-class 'question))
;;    (warn "Question '~A' already exists; ignoring" (third expr)))
  (destructuring-bind (id question name prompt &key help type choices view relations) expr
    (declare (ignore question))
    (register-imported-object 
     (make-instance 'question
		    :name name
		    :prompt prompt
		    :data-help help
		    :type type
		    :choices choices
		    :view (or view :auto)
		    :relations relations)
     id hash)))


