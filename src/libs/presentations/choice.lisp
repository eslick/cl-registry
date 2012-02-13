(in-package :registry)

(defclass member-choice-presentation (web-field-presentation)
  ((choices :initarg :choices
            :reader choices
	    :initform nil
            :documentation "ALIST of (client-text . lisp-value).")
   (empty-option-p :reader empty-option-p 
		   :initarg :empty-option-p 
		   :initform nil
		   :documentation "Do we render a blank option which is 'no answer'?")
   (test-function :initarg :test-function
                  :initform #'equal
                  :reader test-function)))

(defmethod shared-initialize :before ((presentation member-choice-presentation) slot-names &rest args)
  (declare (ignore args slot-names))
  (when (not (slot-boundp presentation 'client-value))
    (setf (slot-value presentation 'client-value) nil)))

(defmethod shared-initialize :after ((presentation member-choice-presentation) slot-names &rest args &key (choices '() choices-p))
  "Normalize the :CHOICES initarg."
  (declare (ignore args slot-names))
  (when choices-p
    (setf (slot-value presentation 'choices)
	  (loop for choice in choices collect
	       (if (atom choice)
		   (cons (princ-to-string choice) choice)
		   choice)))))

(define-lisp-value-getter member-choice-presentation (client-value choices)
  (if (or (null client-value) (eq client-value ""))
      (values nil nil)
      (dolist (choice choices
	       (error "Client string ~S is not a valid value in the choices ~S."
		      client-value choices))
	(when (string= client-value (car choice))
	  (return-from lisp-value (cdr choice))))))

(define-lisp-value-setter member-choice-presentation (lisp-value choices test-function)
  (when (consp lisp-value)
    (setf lisp-value (first lisp-value)))
  (if (null lisp-value)
      nil
      (let ((choice (find lisp-value choices :key #'cdr :test test-function)))
	(unless choice
	  (error "Value ~S not a member of the set ~S."
		 lisp-value (mapcar #'cdr choices)))
	(car choice))))

;;;; * choice type

(defclass member-select-presentation (member-choice-presentation)
  ((multiple-p :initarg :multiple-p :reader multiple-p :initform nil)
   (multiple-hint-p :initarg :multiple-hint-p :reader multiple-hint-p :initform nil)))

(defmethod render-presentation-editable ((presentation member-select-presentation))
  (with-html
    (:select :id (dom-id presentation)
	     :class (css-class presentation)
             :style (css-style presentation)
	     :name (query-name presentation)
	     :multiple (when (multiple-p presentation) "on")
	     :onchange (on-change-validation presentation)
      (let ((client-value (client-value presentation)))
	(dolist (choice (if (empty-option-p presentation)
			    (cons '("" . "") (choices presentation))
			    (choices presentation)))
	  (htm (:option :value (car choice)
			:selected (and client-value 
				       (if (listp client-value)
					   (find (car choice) client-value
						 :test #'string=)
					   (string= client-value (car choice)))
				       "selected")
			(str (find-translation (car choice))))))))
    (when (and (multiple-p presentation) (multiple-hint-p presentation))
      (htm (:p :style "font-size: x-small; margin-top: -5px; margin-left: 10px;"
	       "To select or unselect multiple options, hold the ctrl or command key while clicking")))))

(defmethod update-presentation ((presentation member-select-presentation) args)
  (let ((provided-value (getf-all args (as-argument-keyword (query-name presentation)))))
    (if (and (or (null provided-value)
		 (equal provided-value '(""))
		 (equal provided-value ""))
	     (not (required presentation)))
	(progn (setf (client-value presentation) nil)
	       (values t nil))
	(call-next-method))))


;;;; * multi choice

(defclass multiple-members-select-presentation (member-select-presentation)
  ()
  (:default-initargs :multiple-p t))

(define-lisp-value-getter multiple-members-select-presentation (client-value choices)
  (if (null client-value)
      (values nil t)
      (values (loop for value in (mklist client-value)
		 for choice = (find value choices :key #'car :test #'string=)
		 when choice
		 collect (cdr choice))
	      t)))

(define-lisp-value-setter multiple-members-select-presentation (lisp-value choices test-function)
  (let ((client-values nil))
    (dolist (value (mklist lisp-value))
      (let ((choice (find value choices :key #'cdr :test test-function)))
	(unless choice
	  (error "Value ~S not a member of the set ~S."
		 lisp-value (mapcar #'cdr choices)))
	(push (car choice) client-values)))
    client-values))

;;;; * single choice, radio

(defclass member-radio-presentation (member-choice-presentation)
  ((multiple-p :initarg :multiple-p :reader multiple-p)))

(defmethod render-presentation-editable ((presentation member-radio-presentation))
  (with-html
    (dolist (choice (choices presentation))
      (htm (:input :type "radio" :name (query-name presentation) :value (car choice) 
                   :checked (and (client-value presentation)
                                 (string= (client-value presentation)
					  (car choice)))
		   (str (find-translation (car choice))))))))

;;;; * numeric radio

(defclass number-radio-presentation (member-radio-presentation)
  ((low-value :initarg :low-value :initform 1 :reader low-value)
   (high-value :initarg :high-value :initform 5 :reader high-value)))

(defmethod initialize-instance :after ((presentation number-radio-presentation) &rest initargs)
  (declare (ignore initargs))
  (let (choices)
    (loop for i from (low-value presentation) to (high-value presentation)
       do (push (cons (format nil "~d" i) i) choices))
    (setf (slot-value presentation 'choices) (nreverse choices))))

(define-lisp-value-getter number-radio-presentation (client-value)
  (if client-value
    (parse-integer client-value)
    1))

(define-lisp-value-setter number-radio-presentation (integer)
  (format nil "~D" integer))

(defmethod render-presentation-editable ((presentation number-radio-presentation))
  (call-next-method))