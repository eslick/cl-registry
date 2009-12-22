(in-package :registry)

(registry-proclamations)

(defvar *user-center-alist*
  '((kmcorbett . "Clozure")
    (wws . "Clozure")
    (eslick . "MIT")
    (mnurok . "BWH")
    (jryu . "Mayo")))

(defmodel center ()
  ((short-name :accessor short-name
               :initarg :short-name
               :initform (error "Center short-name is required")
               :index t)
   (name :accessor center-name
         :initarg :name
         :initform (error "Center name is required"))))

(defmethod print-object ((center center) stream)
  (format stream "#<CENTER (~A) '~A'>"
          (object-id center)
          (short-name center)))

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

(defun all-centers ()
  (map-class #'identity 'center :collect t))

(defmodel clinician ()
  ((user :accessor user
         :initarg :user
         :initform (error "Clinician user is required")
         :index t)
   (center :accessor center
           :initarg :center
           :initform (error "Clinician center is required")
           :index t)
   (permissions :initform nil
                :accessor user-permissions ;so permissions functions "just work"
                :set-valued t)))

(defmethod print-object ((clinician clinician) stream)
  (format stream "#<CLINICIAN (~A) '~A' '~A'>"
          (object-id clinician)
          (username (user clinician))
          (short-name (center clinician))))

(defun make-clinician (user center)
  "Returns a clinician for user & center, creating a new one if it doesn't already exist."
  (let ((center (get-center center)))
    (or (get-clinician user center)
        (make-instance 'clinician
                       :user (or (get-user user) (error "Not a user: ~s" user))
                       :center center))))

(defun get-clinician (user center)
  "Gets a clinician for user and center, or NIL if there is none."
  (let ((center (get-center center)))
    (dolist (clinician (get-clinicians-for-user user))
      (when (eq center (center clinician))
        (return clinician)))))

(defun drop-clinician (user center)
  "Drop the clinician record for user and center"
  (awhen (get-clinician user center)
    (drop-instance it)))

(defun get-clinicians-for-user (user)
  (get-instances-by-value 'clinician 'user (get-user user)))

(defun get-centers-for-user (user &optional (only-from-clinicians t))
  (let ((user (get-user user)))
    (if (and (not only-from-clinicians) (has-permission-p user 'admin))
        (map-class 'identity 'center :collect t)
        (mapcar 'center (get-clinicians-for-user user)))))

(defun get-clinicians-for-center (center)
  (get-instances-by-value 'clinician 'center (get-center center)))

(defun get-users-for-center (center)
  (mapcar 'user (get-clinicians-for-center center)))

(defun maybe-current-username ()
  (let ((current-user (ignore-errors (current-user))))
    (and current-user (username current-user))))

(defun current-center ()
  (webapp-session-value 'current-center))

(defun (setf current-center) (center)
  (setf (webapp-session-value 'current-center) (get-center center)))

(defun current-patient ()
  (webapp-session-value 'current-patient))

(defun (setf current-patient) (patient)
  (setf (webapp-session-value 'current-patient) (get-patient patient)))

(defun get-center-from-username (&optional (username (maybe-current-username)))
  (or (cdr (assoc username *user-center-alist* :test #'string-equal)) "UNKNOWN"))

(defun generate-patient-id (&key center sequence)
  (unless center
    (setq center (get-center-from-username (maybe-current-username))))
  (unless sequence
    (setq sequence (symbol-name (gensym))))
  ;; Returns
  (format nil "~A-~A" center sequence))

;;
;; Patients
;;

(defmodel patient ()
  ((id :accessor id
       :initarg :id
       :index t
       :initform (generate-patient-id)
       :documentation "A unique patient ID")
   (center :accessor center
           :initarg :center
           :initform (get-center-from-username)
           :index t
           :documentation "The CENTER for this patient")
   (user :accessor user
         :initarg :user
         :initform (maybe-current-username)
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
  "Get the patient for a patient ID. Error if not found, unless NIL-IF-NONE is true."
  (cond ((typep id 'patient) id)
        ((get-instance-by-value 'patient 'id id))
        ((not nil-if-none)
         (error "There is no patient with an ID of ~s" id))))

(defun get-patient-for-user (user)
  "Get the patient associated with a user, or NIL if there is none."
  (when (stringp user) (setf user (get-user user)))
  (check-type user user)
  (get-instance-by-value 'patient 'user user))

(defun get-patients-for-center (center)
  "Return a list of patients for a CENTER or its short name."
  (get-instances-by-value 'patient 'center (get-center center)))

(defun map-patients-for-center (fn center)
  "Call FN with each patient for CENTER. Return NIL."
  (flet ((mapper (key value)
           (declare (ignore key))
           (funcall fn value)))
    (declare (dynamic-extent #'mapper))
    (map-inverted-index #'mapper 'patient 'center
                        :value (get-center center)
                        :collect nil)))

(defview patient-table-view (:type table
                             :inherit-from '(:scaffold patient))
  )

(defview patient-form-view (:type form :inherit-from '(:scaffold patient))
  (center :hidep t)                     ;get this from the logged-in user
  (user :hidep t)
  )
