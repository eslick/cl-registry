(in-package :registry)

(registry-proclamations)

(defmodel center ()
  ((short-name :accessor short-name
               :initarg :short-name
               :initform nil
               :index t)
   (name :accessor center-name
         :initarg :name
         :initform nil)
   (patient-counter :accessor patient-counter
                    :initform 0)))

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
  (cond ((null short-name) nil)
        ((typep short-name 'center) short-name)
        ((get-instance-by-value 'center 'short-name short-name))
        ((not nil-if-none)
         (error "There is no center with a short-name of ~s" short-name))))

(defun all-centers ()
  (map-class #'identity 'center :collect t))

(defvar *center-counter-lock* (elephant-utils:ele-make-lock))

(defun next-patient-counter (center)
  (check-type center center)
  (elephant-utils:ele-with-lock (*center-counter-lock*)
    (incf (patient-counter center))))

(defview center-table-view (:type table
                           :inherit-from '(:scaffold center))
  )

(defclass center-short-name-parser (parser)
  ()
  (:default-initargs :error-message nil))

(defmethod parse-view-field-value ((parser center-short-name-parser) value obj view field &rest args)
  (declare (ignore obj view field args))
  (cond ((get-center value t)
         (setf (parser-error-message parser)
               (format nil "There is already a center with that Short Name"))
         (return-from parse-view-field-value nil))
        (t (values t t value))))

(defview center-form-view (:type form :inherit-from '(:scaffold center))
  (short-name :parse-as center-short-name)
  (patient-counter :hidep t)
  )

(defmodel clinician ()
  ((user :accessor user
         :initarg :user
         :initform nil
         :index t)
   (center :accessor center
           :initarg :center
           :initform (current-center)
           :index t)
   (center-admin-p :accessor center-admin-p
                   :initarg :center-admin-p
                   :initform nil
                   :type boolean)))

(defmethod print-object ((clinician clinician) stream)
  (format stream "#<CLINICIAN (~A) '~A' '~A'>"
          (object-id clinician)
          (username (user clinician))
          (short-name (center clinician))))

(defview clinician-table-view (:type table
                                     :inherit-from '(:scaffold clinician))
  (user :reader 'clinician-username :label #!"User")
  (center :reader 'clinician-center-short-name :label #!"Center")
  (center-admin-p :present-as predicate
                  :label #!"Center Admin?")
  )

(defclass clinician-user-parser (parser)
  ()
  (:default-initargs :error-message nil))

(defmethod parse-view-field-value ((parser clinician-user-parser) value obj view field &rest args)
  (declare (ignore view field args))
  (let ((user (get-user value))
        (center (if obj (center obj) (current-center))))
    (cond (user
           (dolist (clin (get-clinicians-for-user user))
             (when (and (eq center (center clin)) (not (eq clin obj)))
               (setf (parser-error-message parser)
                     (format nil "There is already a clinician for that user"))
               (return-from parse-view-field-value nil)))
           (values t t user))
          (t (setf (parser-error-message parser)
                   (format nil "~s is not a valid username" value))
             nil))))

(defview clinician-form-view (:type form :inherit-from '(:scaffold clinician))
  (user :reader 'clinician-username :parse-as clinician-user)
  (center :hidep t)
  )

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

(defun current-clinician ()
  (get-clinician (current-user t) (current-center)))

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
  (and (boundp 'hunchentoot:*session*)
       (webapp-session-value 'current-center)))

(defun (setf current-center) (center)
  (setf (webapp-session-value 'current-center) (get-center center)))

(defpclass current-patient ()
  ((user :accessor current-patient-user
         :initarg :user
         :initform (current-user t)
         :index t)
   (patient :accessor current-patient-patient
            :initarg :patient
            :initform nil)))

(defun current-patient ()
  (and (boundp 'hunchentoot:*session*)
       (webapp-session-value 'current-patient)))

(defmethod print-object ((o current-patient) stream)
  (print-unreadable-object (o stream :type t)
    (let ((user (current-patient-user o))
          (patient (current-patient-patient o)))
      (format stream "~a ~a"
              (and user (username user))
              (and patient (id patient))))))

(defun get-current-patient-for-user (user &optional create patient)
  (setf user (get-user user))
  (let ((res (get-instance-by-value 'current-patient 'user user)))
    (cond (res
           (when patient (setf (current-patient-patient res) patient))
           res)
          (create (make-instance 'current-patient
                                 :user user
                                 :patient patient))
          (t nil))))

(defun (setf current-patient) (patient)
  (let* ((patient (get-patient patient))
         (user (current-user t)))
    (setf (webapp-session-value 'current-patient) patient)
    (when user
      (get-current-patient-for-user user t patient))
    patient))

(defun initialize-current-patient ()
  (let ((user (current-user t)))
    (when user
      (let* ((current-patient (get-current-patient-for-user user t))
             (patient (current-patient-patient current-patient))
             (clinician nil))
        (unless patient
          (setf clinician (car (get-clinicians-for-user user))
                patient (and clinician
                             (car (get-patients-for-center
                                   (center clinician))))))
        (when patient
          (setf (current-patient) patient
                (current-center) (center patient)))
        patient))))

(defun generate-patient-id (&key center sequence)
  (unless center
    (setq center (current-center)))
  (check-type center center)
  (unless sequence
    (setq sequence (next-patient-counter center)))
  ;; Returns
  (format nil "~A-~3,'0d" (short-name center) sequence))

;;
;; Patients
;;

(defmodel patient ()
  ((id :accessor id
       :initarg :id
       :index t
       :initform (unless *importing-model-objects* (generate-patient-id))
       :documentation "A unique patient ID")
   (center :accessor center
           :initarg :center
           :initform (current-center)
           :index t
           :documentation "The CENTER for this patient")
   (external-id :accessor external-id
                :initarg :external-id
                :initform nil
                :index t)
   (user :accessor user
         :initarg :user
         :initform nil
         :index t
         :documentation "If this patient is a user, the USER instance")))

(defmethod print-object ((o patient) stream)
  (print-unreadable-object (o stream :type t)
    (format stream "oid:~a ~a ~a"
            (object-id o)
            (id o)
            (short-name (center o)))))

(defun make-patient (id center &optional user)
  (when (stringp user) (setf user (get-user user)))
  (check-type id string)
  (check-type center (or string center))
  (check-type user (or null user))
  (setq center (get-center center))
  (assert (null (get-patient id center t))
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
                    :id id :center center :user user)))
      (make-provenance patient :user (current-user t) :center center)
      patient)))

(defun get-patient (id &optional center nil-if-none)
  "Get the patient for a patient ID. Error if not found, unless NIL-IF-NONE is true."
  (let ((center (get-center (or center (current-center)))))
    (cond ((null id) nil)
          ((typep id 'patient) id)
          ((let ((patients (get-instances-by-value 'patient 'id id)))
             (if (and patients (null (cdr patients)) (null center))
                 (car patients)
                 (dolist (patient patients)
                   (when (eq (center patient) center) (return patient))))))
          ((not nil-if-none)
           (error "There is no patient with an ID of ~s" id)))))

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

(defun patient-center-short-name (patient)
  (awhen (center patient)
    (short-name it)))

(defun patient-username (patient)
  (awhen (user patient)
    (username it)))

(defview patient-table-view (:type table
                             :inherit-from '(:scaffold patient))
  (center :reader 'patient-center-short-name)
  (user :hidep t :reader 'patient-username)
  )

(defclass patient-id-parser (parser)
  ()
  (:default-initargs :error-message nil))

(defmethod parse-view-field-value ((parser patient-id-parser) value obj view field &rest args)
  (declare (ignore view field args))
  (let* ((center (current-center))
         (patient (get-patient value center t)))
    (cond ((and patient (not (eq patient obj)))
           (setf (parser-error-message parser)
                 (format nil "There is already a patient with that Id"))
               (return-from parse-view-field-value nil))
          (t (values t t value)))))

(defview patient-form-view (:type form :inherit-from '(:scaffold patient))
  (id :parse-as patient-id)
  (center :hidep t)                  ;use (current-center)
  (user :hidep t)                    ;get this from the logged-in user
  )
