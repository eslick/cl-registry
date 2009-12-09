(in-package :registry)

(registry-proclamations)

(defun/cc do-change-password-dialog ()
  (do-dialog "" (make-instance 'change-password)))

(defwidget change-password (composite)
  ((quickform :accessor change-password-quickform :initarg :quickform :initform nil)
   (flash :accessor change-password-flash :initarg :flash
	  :initform (make-instance 'flash))
   (view :accessor change-password-view :initarg :view :initform 'change-password-view)))

(defview change-password-view (:type form :persistp nil
			       :caption #!"Change password"
			       :buttons `((:submit . ,#!"Submit")
					  (:cancel . ,#!"Cancel")))
  (old-password :present-as password :requiredp t)
  (new-password :present-as password :requiredp t)
  (confirm-password :present-as password :requiredp t))

(defmethod initialize-instance :after ((widget change-password) &rest initargs)
  (declare (ignore initargs))
  (setf (change-password-quickform widget)
	(make-quickform (change-password-view widget)
			:on-success (lambda (w data)
				      (declare (ignore w))
				      (with-slots (new-password) data
					(setf (user-password (current-user))
					      (create-sha1-password new-password)))
				      (flash-message (change-password-flash widget)
						     #!"Password updated."))
			:satisfies (lambda (w data)
				     (declare (ignore w))
				     (validate-password-change data))
			:answerp t))
  (setf (composite-widgets widget)
	(list (change-password-flash widget)
	      (change-password-quickform widget))))

(defun validate-password-change (data)
  ;; XXX copy-n-paste from new-register.lisp
  (flet ((find-field-for-slot-name (slot-name fields)
	   ;; This is a bit of a mess.  We have to return an
	   ;; a-list of fields and error messagses, but it's not
	   ;; as convenient as one would like to get one's hands
	   ;; on the field objects.
	   ;; fields: a list of field-info structures
	   ;; f: the field slot in said structure
	   ;; view-field-slot-name: slot in f (field object)
	   (field-info-field
	    (find slot-name fields
		  :key (lambda (f)
			 (let ((field (field-info-field f)))
			   (view-field-slot-name field)))))))
    (let* ((fields (get-object-view-fields data 'change-password-view))
	   (old-password-field (find-field-for-slot-name 'old-password fields))
	   (password-field (find-field-for-slot-name 'new-password fields))
	   (confirm-password-field (find-field-for-slot-name 'confirm-password fields))
	   (user (current-user))
	   (alist nil))
      (with-slots (old-password new-password confirm-password) data
	(unless (validate-sha1-password old-password (user-password user))
	  (push (cons old-password-field #!"password incorrect") alist))
	(when (not (equal new-password confirm-password))
	  (push (cons password-field #!"passwords must match") alist)
	  (push (cons confirm-password-field #!"passwords must match") alist)))
      (if alist
	(values nil alist)
	t))))
