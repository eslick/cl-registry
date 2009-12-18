(in-package :registry)

(registry-proclamations)

(defmodel center ()
  ((short-name :accessor short-name
               :initarg :short-name
               :initform (error "Center short-name is required")
               :index t)
   (name :accessor center-name
         :initarg :name
         :initform (error "Center name is required"))))

(defun make-center (short-name name)
  (check-type short-name string)
  (check-type name string)
  (assert (null (get-center short-name t))
          nil
          "There is already a center with a short name of ~s"
          short-name)
  (make-instance 'center :short-name short-name :name name))

(defun get-center (short-name &optional nil-if-none)
  (cond ((typep short-name 'center) short-name)
        ((get-instance-by-value 'center 'short-name short-name))
        ((not nil-if-none)
         (error "There is no center with a short-name of ~s" short-name))))

(defmodel patient ()
  ((id :accessor id
       :initarg :id
       :index t
       :initform (error "Patient ID is required")
       :documentation "A unique patient ID")
   (center :accessor center
           :initarg :center
           :initform nil
           :index t
           :documentation "The CENTER for this patient")
   (user :accessor user
         :initarg :user
         :initform nil
         :index t
         :documentation "If this patient is a user, the USER instance")))

(defun make-patient (id center &optional user)
  (when (stringp user) (setf user (get-user user)))
  (check-type id string)
  (check-type center (or string center))
  (check-type user (or null user))
  (assert (null (get-patient id))
          nil
          "Patient already exists with id ~s"
          id)
  (assert (or (null user) (null (get-patient-for-user user)))
          nil
          "Patient already exists for user ~s"
          user)
  (with-transaction ()
    (let ((patient (make-instance
                    'patient
                    :id id :center (get-center center) :user user)))
      (make-provenance patient :user (current-user t) :center center)
      patient)))

(defun get-patient (id &optional nil-if-none)
  (cond ((typep id 'patient) id)
        ((get-instance-by-value 'patient 'id id))
        ((not nil-if-none)
         (error "There is no patient with an ID of ~s" id))))

(defun get-patient-for-user (user)
  (when (stringp user) (setf user (get-user user)))
  (check-type user user)
  (get-instance-by-value 'patient 'user user))

(defun get-patients-for-center (center)
  (get-instances-by-value 'patient 'center (get-center center)))

(defun map-patients-for-center (fn center)
  (flet ((map-fn (key value)
           (declare (ignore key))
           (funcall fn value)))
    (declare (dynamic-extent #'map-fn))
    (map-inverted-index #'map-fn 'patient 'center
                        :value (get-center center)
                      :collect nil)))
