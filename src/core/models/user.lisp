(in-package :registry)

(registry-proclamations)

(defmodel user ()
  ((username :accessor username
	     :initarg :username
	     :initform nil
	     :index t)
   (password :accessor user-password
	     :initarg :password
	     :initform nil)
   (first-name :initform nil
	       :accessor first-name
	       :initarg :first-name)
   (last-name :initform nil
	      :accessor last-name
	      :initarg :last-name)
   (email :accessor user-email
	  :initarg :email
	  :type string
	  :index t)
   (date-joined :accessor user-date-joined
		:initarg :joined
		:initform (get-universal-time))
   (permissions :initform nil
		:accessor user-permissions
		:set-valued t)
   (preferences-btree :initform (make-btree)
		      :accessor user-preferences-btree
		      :initarg :preferences-btree)
   (ratings :accessor user-ratings
	    :initarg :ratings
	    :initform nil
	    :documentation "keyed by objects that the user
                            has rated")
   (locale :accessor user-locale
           :initarg :locale
           :initform nil
           :documentation "Prefered locale for this user."))
  (:documentation "The main user account object"))

(defmethod print-object ((user user) stream)
  (format stream "#<USER (~A) '~A'>" (object-id user) (username user)))

(defun get-user (username)
  "Convenience function to lookup a user instance regardless of what it
   is: string = username, user object = identity, by ID"
  (cond ((subtypep (type-of username) 'user)
	 username)
	((numberp username)
	 (get-model 'user username))
	((find #\@ username)
	 (get-instance-by-value 'user 'email username))
	(t (get-instance-by-value 'user 'username username))))

(defun drop-user (user)
  "How to handle user 'unregistration'"
  (drop-instances (get-instances-by-value 'answer 'user user))
  (drop-instances (get-instances-by-value 'survey-state 'user user))
  (awhen (get-patient-for-user user)
    (drop-instance it))
  (drop-instance user))


;;
;; Administrative interface to user objects
;;

;; Table view

(defview user-table-view (:type table :inherit-from '(:scaffold user))
  (password :hidep t)
  (date-joined :hidep t)
  (permissions :hidep t)
;;  (address :hidep t)
  (ratings :hidep t))

;; Data view
(defview user-data-view (:type data
			 :inherit-from '(:scaffold user))
  (password :present-as password)
  (date-joined :hidep t)
  (permissions :hidep t)
;;  (address :type mixin :view '(data address))
  (ratings :hidep t))

;; Form view
(defview user-form-view (:type form
			 :inherit-from '(:scaffold user))
  (username :requiredp t)
  (password :requiredp t
	    :present-as password)
;;  (confirm-password :requiredp t
;;		    :present-as password
;;		    :slot-name password)
  (date-joined :hidep t)
  (permissions :hidep t)
;;  (address :type mixin :view 'address-form-view)
  (ratings :hidep t))
   
	     
(defun all-users ()
  (find-persistent-objects *default-store* 'user))

(defun users-with-permission (permission &optional specific-p)
  "Return a list of users satisfying (has-permissions-p permission specific-p)"
  (when (symbolp permission)
    (setf permission (get-permission permission)))
  (let ((res nil))
    (when permission
      (map-class (lambda (user)
                   (when (has-permission-p user permission specific-p)
                     (push user res)))
                 'user))
    res))

(defun all-user-oids ()
  (map-class #'identity 'user :collect t :oids t))
	     
(defun user-add (name pw &key first last email)
  (make-instance 'user
		 :username name
		 :password (create-sha1-password pw)
		 :first-name (or first "")
		 :last-name (or last "")
		 :email (or email "")))

(defmethod user-locale ((user null))
  (cl-l10n:locale "en_US"))

(defmethod user-locale :around ((user user))
  "Infer the users locale from either the locale slot or their
language and country preferences."
  (let ((locale (call-next-method)))
    (if locale
        (cl-l10n:locale locale)
        (let ((language (or (get-preference :default-language user)
                            (session-language)))
              (country (get-preference :residence-country user)))
          (flet ((set-locale (locale-name)
                   (handler-case 
                       (progn
                         (if (cl-l10n:locale locale-name)
                             (setf (slot-value user 'locale) locale-name)
                             (setf (slot-value user 'locale) "en_US"))
                         (cl-l10n:locale (slot-value user 'locale)))
                     (error () 
                       (setf (slot-value user 'locale) "en_US")
                       (cl-l10n:locale "en_US")))))
            (cond
              ((and language country)
               (set-locale (format nil "~A_~A"
                                   (string-downcase language)
                                   (string-upcase country))))
              (language
               (set-locale (string-downcase language)))
              (country
               ;; FIXME: this is so wrong
               (macrolet ((locale-country-case (&rest clauses)
                            `(cond ,@(mapcar (lambda (clause)
                                               `((string-equal country ,(first clause))
                                                 (set-locale ,(second clause))))
                                             clauses))))
                 (locale-country-case
                  ("US" "en_US")
                  ("DE" "de_DE")
                  ("IT" "it_IT")
                  ("FR" "fr_FR"))))
              (t (set-locale "en_US"))))))))
