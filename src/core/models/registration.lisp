(in-package :registry)

(defmodel registration-request ()
  ((username :accessor reg-username :initarg :username)
   (password :accessor reg-pw :initarg :password)
   (email :accessor reg-email :initarg :email)
   (magic-key :accessor reg-magic-key :initarg :magic-key)
   (date :accessor reg-date :initform (get-universal-time)))
  (:documentation "User information for a pending registration.
                   After registration confirmation, these
                   objects are deleted."))

;;; Views for the admin page

(defview registration-request-table-view (:type table)
  username
  email
  magic-key)

(defview registration-request-data-view (:type data)
  username
  password
  email
  magic-key
  date)

(defview registration-request-form-view (:type form)
  (username :requiredp t)
  (password :requiredp t)
  (email :requiredp t)
  (magic-key :requiredp t))
