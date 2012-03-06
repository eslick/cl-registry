(in-package :registry)

(defvar *models* (make-hash-table)
  "Keeps track of all the models that have been created using defmodel and
   creates an admin model to generate widgets on demand")

(defvar *registered-models* (make-hash-table)
  "Keeps track of all the models that have been created using defmodel and
   ensures that an editing widget has been created for it")

(defconstant $MID-ROOT-KEY 
  (if (boundp '$MID-ROOT-KEY)
      (symbol-value '$MID-ROOT-KEY)
      "*LAMSIGHT-MID*"))

(defvar *mid-lock* (elephant-utils:ele-make-lock))
(defvar *mid* nil)
(defparameter *mid-period* 100)
(defvar *last-mid-write* nil)

;; get-from-root + add-to-root takes 75 milliseconds, so we only
;; write once every *mid-period* times.
(defun next-mid (&optional min)
  (elephant-utils:ele-with-lock (*mid-lock*)
    (when (null *mid*)
      (setf *mid* (or (get-from-root $MID-ROOT-KEY)
                      (elephant::next-oid
                       (weblocks-elephant::elephant-controller *registry-main*)))))
    (prog1
        (setf *mid* (max (1+ *mid*) (or min 0)))
      (when (or (null *last-mid-write*)
                (>= *mid* *last-mid-write*))
        (setf *last-mid-write*
              (add-to-root $MID-ROOT-KEY (+ *mid* *mid-period*)))))))

(defpclass weblocks-model ()
  ((mid :writer (setf mid)              ;reader below
        :initarg :mid
        :initform nil
        :index t
        :documentation "The model ID of the instance.
                        Preserved across export/import.")))

(defmethod mid ((instance weblocks-model))
  (or (and (slot-boundp instance 'mid) ;bootstrapping
           (slot-value instance 'mid))
      ;; For existing model instances.
      (object-id instance)))

(defmethod mid ((instance persistent))
  (object-id instance))

(defmethod mid ((instance null))
  nil)

(defmethod initialize-instance :after ((instance weblocks-model) &rest initargs)
  (declare (ignore initargs))
  (let ((mid (slot-value instance 'mid)))
    (if mid
        (when (or (null *mid*) (> mid *mid*)) (next-mid mid))
        (setf (mid instance) (next-mid)))))

(defun get-model (class-name mid &optional (store *registry-main*))
  (or (get-instance-by-value class-name 'mid mid)
      ;; For objects that existed before the MID slot was added
      (let ((res (find-persistent-object-by-id store class-name mid)))
        (and res
             (eq (type-of res) class-name)
             (eql (mid res) mid)
             res))))
  

(defmacro defmodel (name superclasses &rest args)
  `(progn 
     (defpclass ,name ,(ensure-member 'weblocks-model superclasses) ,@args)
     (setf (gethash ',name *models*) nil)
     (when (gethash ',name *registered-models*)
       (setf (gethash ',name *registered-models*) nil))))

(defun ensure-member (item list)
  (if (find item list) list
      (append list (list item))))

(defun make-default-instance (classname)
  "Creates a default instance of a model: Elephant specific"
  (assert (subtypep classname 'persistent))
  (make-instance (weblocks-elephant::return-proxy-classname classname)))

;;; Model id parser
(defclass mid-parser (parser)
  ((class-name :initform nil
	       :initarg :class-name
	       :accessor mid-parser-class-name
	       :documentation "A class of the object whose id is being
	       parsed."))
  (:default-initargs :error-message nil)
  (:documentation "A parser designed to convert a model id (MID) into an
  object instance."))

(defmethod parser-error-message ((parser mid-parser))
  (with-slots (error-message) parser
    (or error-message
	(format nil "This value must be a valid ~A"
		(string-downcase
		 (humanize-name
		  (mid-parser-class-name parser)))))))

(defmethod parse-view-field-value ((parser mid-parser) value obj
				   (view form-view) (field form-view-field) &rest args)
  (declare (ignore args obj))
  (unless (text-input-present-p value)
    (return-from parse-view-field-value (values t nil)))
  (let ((object
	 (get-model (mid-parser-class-name parser)
                    (parse-integer value :junk-allowed nil)
                    (class-store (mid-parser-class-name parser)))))
    (when object
      (values t t object))))
