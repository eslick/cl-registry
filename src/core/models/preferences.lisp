(in-package :registry)

;;; preferences are stored as a btree in the user object.
;;; the keys are strings naming the preference.

;; =============================================================================
;; Accessing user preferences in the current user's btree
;; =============================================================================

(defmethod get-preference (name (user user))
    "look up name in the current user's preferences.  if a value is found,
   t is returned as the second value.  if nothing is found for key, nil
   is returned as the second value."
    (get-value name (user-preferences-btree user)))

(defun current-user-preference (name)
  (get-preference name (current-user)))

(defun has-preference-value-p (user name value)
  (equal (get-preference name user) value))

(defmethod (setf get-preference) (value name (user user))
  (setf (get-value name (user-preferences-btree user)) value))

(defun get-preferences (user)
  (flatten1
   (map-btree (lambda (k v) (list k v))
	      (user-preferences-btree user)
	      :collect t)))

(defun users-for-preference-value (preference value)
  "Filter users by a preference value"
  (let ((users nil))
    (map-class (lambda (user)
		 (when (equal (get-preference preference user) value)
		   (push user users)))
	       'user)
    users))

(defun users-for-preference-values (&rest plist)
  (let ((users nil)
	(pairs (pairs plist)))
    (map-class (lambda (user)
		 (when (every (lambda (pair)
				(equal (get-preference (car pair) user) (cdr pair)))
			      pairs)
		   (push user users)))
	       'user)
    users))

(defun counts-for-preference-selections (&rest preferences)
  (with-transaction ()
    (loop for preference in preferences
       collect (list preference (length (funcall #'users-for-preference-values 
						 preference t))))))

;; =============================================================================
;;  Programmer preference access (using keywords)
;; =============================================================================

(defclass user-preference ()
  ((name :accessor name :initarg :name
         :documentation "the name of the preference, a keyword. must
         be unique across all preferences.")
   (prompt :accessor prompt :initarg :prompt)
   (presentation-maker :accessor presentation-maker :initarg :presentation-maker
                       :documentation "a zero argument function which
                       generates a new presentation object for this
                       preference. this function must have no side
                       effects (apart from allocating memory
                       obviously) and may be called an arbitrary
                       number of times."))
  (:documentation "represents a class of user preferences. these
  objects are stored in the *preferences* table and used to create the
  right presentation objects when rendering the user-preference
  widget."))

(defvar *preferences* (make-hash-table)
  "a hash mapping preference names (keywords) to user-preference objects")

(defun print-all-preferences ()
  (map-class (lambda (pref)
	       (print (name pref)))
	     'user-preference))

;; =============================================================================
;;  Macrology
;; =============================================================================

(defmacro def-preference (name (presentation) &body presentation-args &key prompt &allow-other-keys)
  (let ((name (kintern name)))
    `(eval-when (:load-toplevel :execute)
       (setf (gethash ',name *preferences*)
             (make-instance 'user-preference
                            :name ',name
			    :prompt ,prompt
                            :presentation-maker
                            (lambda ()
                              (make-instance ',presentation
                                             ,@presentation-args
                                             :query-name ,(string-downcase (symbol-name name))))))
       ',name)))

;; =============================================================================
;;  Form handling
;; =============================================================================

(defun set-user-preferences-from-widget (user-preferences-widget args)
  (mapcar #'(lambda (name)
	      (let ((presentation (gethash name (preference-presentations
						 user-preferences-widget))))
		(when (or (getf args name)
			  (eq (type-of presentation) 'checkbox-boolean-presentation)
			  (eq (type-of presentation) 'multiple-members-select-presentation))
		  (setf (client-value presentation) (getf-all args name))
		  (let ((val (lisp-value presentation)))
		    (unless (eq val :none)
		      (setf (get-preference name (current-user)) 
			    (lisp-value presentation))))
		  ;; Special case
		  (when (and (eq name :default-language)
			     (not (equal (lisp-value presentation) (session-language))))
		    (change-language-handler :language (lisp-value presentation))
		    (setf (refresh-on-close-p user-preferences-widget) t)))))
	  (prior-rendered-preferences user-preferences-widget)))

(defun preference-form-handler (user-preferences-widget)
  "Handle OK on a user-preferences-widget.

  Preferences will be updates as the user changes them, provided
  that JavaScript is enabled.  Otherwise, OK is the only option,
  and it will commit the changes.

  We loop over all the presentations in the user-preferences-widget
  and set the corresponding user-preference, in (current-user), to the
  lisp value."
  (lambda (&rest args &key done &allow-other-keys)
    (set-user-preferences-from-widget user-preferences-widget args)
    (when done
      (when (refresh-on-close-p user-preferences-widget)
	;; refresh the current page if the user changed the language
	(post-action-redirect (request-uri-path)))
      (answer user-preferences-widget))))

;; =============================================================================
;; Rendering preferences
;; =============================================================================

(defun present-preferences (&rest names)
  (declare (special *current-widget*))
  (dolist (name names)
    (let ((presentation (gethash name (preference-presentations *current-widget*))))
      (pushnew name (prior-rendered-preferences *current-widget*))
      (with-html 
	(:div :class (format nil "preference-object ~A" (string-downcase (type-of presentation)))
	      (:span :class "pref-label"
		     (render-prompt presentation))
	      (:span :class "pref-form"
		     (render-presentation-editable presentation)))))))

;; =============================================================================
;; Preference definitions
;; =============================================================================

(def-preference forum-subscriber (checkbox-boolean-presentation)
  :prompt #!"Receive e-mail from discussion forums?"
  :lisp-value t)

(def-preference forum-email-frequency (member-select-presentation)
  :prompt #!"How frequently do you want forum updates?"
  :choices `((,#!"Daily" . "daily")
	     (,#!"Weekly" . "weekly")
	     (,#!"Monthly" . "monthly"))
  :test-function #'string-equal
  :lisp-value "weekly")

(def-preference update-subscriber (checkbox-boolean-presentation)
  :prompt #!"Receive site updates or reminders?"
  :lisp-value t)

(def-preference update-email-frequency (member-select-presentation)
  :prompt #!"How frequently do you want updates and reminders?"
  :choices `((,#!"Weekly" . "weekly")
	     (,#!"Monthly" . "monthly"))
  :test-function #'string-equal
  :lisp-value "monthly")

(def-preference contact-to-authenticate (checkbox-boolean-presentation)
  :prompt #!"To confirm my identity"
  :lisp-value t)

(def-preference contact-for-study (checkbox-boolean-presentation)
  :prompt #!"For a potential study"
  :lisp-value t)

(def-preference contact-for-data-verification (checkbox-boolean-presentation)
  :prompt #!"To follow up and verify any data I have entered"
  :lisp-value t)

(def-preference contact-methods (multiple-members-select-presentation)
  :prompt #!"By any of these methods"
  :choices (list
	    (cons #!"Electronic mail" "email")
	    (cons #!"Physical Mail" "mail")
	    (cons #!"Phone call" "phone")
 	    (cons #!"Through another patient" "another patient"))
  :test-function #'string-equal
  :lisp-value '("email"))

(def-preference never-contact (checkbox-boolean-presentation)
  :prompt #!"Never contact me by any means"
  :lisp-value nil)

(def-preference lam-patient-p (checkbox-boolean-presentation)
  :prompt #!"I am a LAM patient."
  :lisp-value t)

(def-preference tsc-patient-p (checkbox-boolean-presentation)
  :prompt #!"I am a TSC patient without LAM.")

(def-preference family-member-p (checkbox-boolean-presentation)
  :prompt #!"I am a family member of a patient.")

(def-preference clinician-p (checkbox-boolean-presentation)
  :prompt #!"I am a LAM clinican.")

(def-preference lam-researcher-p (checkbox-boolean-presentation)
  :prompt #!"I work directly on LAM research.")

(def-preference researcher-p (checkbox-boolean-presentation)
  :prompt #!"I am involved in disease research.")

(def-preference other-researcher-p (checkbox-boolean-presentation)
  :prompt #!"I work in another area of research.")

(def-preference unknown-role-p (checkbox-boolean-presentation)
  :prompt #!"I am none of the above.")

(def-preference family-member-patient (short-string-presentation)
  :prompt #!"Their LAMsight username:")

(def-preference first-name (short-string-presentation)
  :prompt #!"First Name: ")

(def-preference last-name (short-string-presentation)
  :prompt #!"Last Name: ")

(def-preference residence-addr-1 (short-string-presentation)
  :prompt #!"Address: ")

(def-preference residence-addr-2 (short-string-presentation)
  :prompt #!"Address 2: ")

(def-preference residence-city (short-string-presentation)
  :prompt #!"City: ")

(def-preference residence-prov (short-string-presentation)
  :prompt #!"State/Province: ")

(def-preference residence-country (member-select-presentation)
  :prompt #!"Country: "
  :choices *iso-country-codes*
  :lisp-value "us"
  :test-function #'string-equal)

(def-preference postal-code (short-string-presentation)
  :prompt #!"Postal code: ")

(def-preference home-phone (short-string-presentation)
  :prompt #!"Home Phone #: ")

(def-preference work-phone (short-string-presentation)
  :prompt #!"Work Phone #: ")

(def-preference default-language (member-select-presentation)
  :prompt #!"Preferred Language: "
  :choices *language-defs*
  :lisp-value "en"
  :test-function #'string-equal)

(def-preference enable-laura-p (checkbox-boolean-presentation)
  :prompt #!"Enable the interactive agent Laura"
  :lisp-value nil)

(def-preference explorer-plugin (member-select-presentation)
  :prompt #!"Explorer Version: "
  :choices `((,#!"Default" . nil)
	     (,#!"FlashEx" . "flash-explorer"))
  :lisp-value nil
  :test-function #'equal)
  

(def-preference map-me-p (checkbox-boolean-presentation)
  :prompt #!"Allow my city, state and country information to be used on the patient map."
  :lisp-value t)

;;
;; Specialized preferences
;;

(def-preference estrogen-study-reminders-enabled-p (checkbox-boolean-presentation)
  :prompt #!"Enable daily reminders"
  :lisp-value nil)

(def-preference estrogen-study-reminder-time (time-presentation)
  :prompt #!"Time of day")

(def-preference estrogen-study-reminder-zone (member-select-presentation)
  :prompt #!"Your home time zone"
  :choices (time-zone-select)
  :lisp-value nil)

;;
;; Some preference-specific predicates
;;

(defun researcher-p (user)
  (some (lambda (pref) (has-preference-value-p user pref t))
	'(:lam-researcher-p :clinician-p :other-researcher-p :researcher-p)))
 
(defgeneric patient-p (obj)
  (:documentation "Returns non-NIL if OBJ is a patient, self-identified by preference")
  (:method ((obj t)) nil))

(defmethod patient-p ((patient patient))
  (awhen (user patient)
    (patient-p it)))

(defmethod patient-p ((user user))
  (some (lambda (pref) (has-preference-value-p user pref t))
	'(:lam-patient-p :tsc-patient-p)))


(defun time-zone-select2 ()
  (sort 
   (mapcar (lambda (entry)
			 (destructuring-bind (location . tz) entry
			   (cons (format nil "~A" location (local-time::timezone-name tz))
					 location)))
		   (hash-items local-time::*location-name->timezone*))
   #'string>
   :key #'car))

(defun time-zone-offset (tz)
  (third (find tz *time-zones* :key #'first :test #'equal)))

(defun time-zone-select ()
  (mapcar (lambda (entry)
			(cons (first entry) (first entry)))
		  *time-zones*))

(defparameter *time-zones*
  '(("Australian Central Standard Time" "ACST" "+09:30")
    ("ASEAN Common Time" "ACT" "+08")
    ("Australian Eastern Standard Time" "AEST" "+10")
    ("Afghanistan Time" "AFT" "+04:30")
    ("Alaska Standard Time" "AKST" "-09")
    ("Armenia Time" "AMT" "+04")
    ("Argentina Time" "ART" "-03")
    ("Arab Standard Time (Kuwait, Riyadh)" "AST" "+03")
    ("Arabian Standard Time (Abu Dhabi, Muscat)" "AST" "+04")
    ("Arabic Standard Time (Baghdad)" "AST" "+03")
    ("Atlantic Standard Time" "AST" "-04")
    ("Australian Western Standard Time" "AWST" "+08")
    ("Azores Standard Time" "AZOST" "-01")
    ("Azerbaijan Time" "AZT" "+04")
    ("Brunei Time" "BDT" "+08")
    ("British Indian Ocean Time" "BIOT" "+06")
    ("Baker Island Time" "BIT" "-12")
    ("Bolivia Time" "BOT" "-04")
    ("Brasilia Time" "BRT" "-03")
    ("Bangladesh Standard Time" "BST" "+06")
    ("Bhutan Time" "BTT" "+06")
    ("Central Africa Time" "CAT" "+02")
    ("Cocos Islands Time" "CCT" "+06:30")
    ("Central European Time" "CET" "+01")
    ("Chatham Standard Time" "CHAST" "+12:45")
    ("Clipperton Island Standard Time" "CIST" "-08")
    ("Cook Island Time" "CKT" "-10")
    ("Chile Standard Time" "CLT" "-04")
    ("Colombia Time" "COT" "-05")
    ("Central Standard Time (North America)" "CST" "-06")
    ("China Standard Time" "CST" "+08")
    ("Central Standard Time (Australia)" "CST" "+09:30")
    ("China Time" "CT" "+08")
    ("Cape Verde Time" "CVT" "-01")
    ("Christmas Island Time" "CXT" "+07")
    ("Chamorro Standard Time" "CHST" "+10")
    ("Easter Island Standard Time" "EAST" "-06")
    ("East Africa Time" "EAT" "+03")
    ("Ecuador Time" "ECT" "-05")
    ("Eastern European Time" "EET" "+02")
    ("Eastern Standard Time (North America)" "EST" "-05")
    ("Fiji Time" "FJT" "+12")
    ("Falkland Islands Time" "FKT" "-04")
    ("Galapagos Time" "GALT" "-06")
    ("Georgia Standard Time" "GET" "+04")
    ("Greenwich Mean Time" "GMT" "")
    ("South Georgia and the South Sandwich Islands" "GST" "-02")
    ("Gulf Standard Time" "GST" "+04")
    ("Guyana Time" "GYT" "-04")
    ("Hawaii-Aleutian Standard Time" "HAST" "-10")
    ("Hong Kong Time" "HKT" "+08")
    ("Heard and McDonald Islands Time" "HMT" "+05")
    ("Hawaii Standard Time" "HST" "-10")
    ("Indochina Time" "ICT" "+07")
    ("Irkutsk Time" "IRKT" "+08")
    ("Iran Standard Time" "IRST" "+03:30")
    ("Indian Standard Time" "IST" "+05:30")
    ("Israel Standard Time" "IST" "+02")
    ("Japan Standard Time" "JST" "+09")
    ("Krasnoyarsk Time" "KRAT" "+07")
    ("Korea Standard Time" "KST" "+09")
    ("Lord Howe Standard Time" "LHST" "+10:30")
    ("Line Islands Time" "LINT" "+14")
    ("Magadan Time" "MAGT" "+11")
    ("Marquesas Islands Time" "MIT" "-09:30")
    ("Moscow Standard Time" "MSK" "+03")
    ("Malaysian Standard Time" "MST" "+08")
    ("Mountain Standard Time (North America)" "MST" "-07")
    ("Myanmar Standard Time" "MST" "+06:30")
    ("Mauritius Time" "MUT" "+04")
    ("Malaysia Time" "MYT" "+08")
    ("Norfolk Time[1]" "NFT" "+11:30")
    ("Nepal Time" "NPT" "+05:45")
    ("Newfoundland Standard Time" "NST" "-03:30")
    ("Newfoundland Time" "NT" "-03:30")
    ("New Zealand Standard Time" "NZST" "+12")
    ("Omsk Time" "OMST" "+06")
    ("Kamchatka Time" "PETT" "+12")
    ("Phoenix Island Time" "PHOT" "+13")
    ("Pakistan Standard Time" "PKT" "+05")
    ("Pacific Standard Time (North America)" "PST" "-08")
    ("Philippine Standard Time" "PST" "+08")
    ("Reunion Time" "RET" "+04")
    ("Samara Time" "SAMT" "+04")
    ("South African Standard Time" "SAST" "+02")
    ("Solomon Islands Time" "SBT" "+11")
    ("Seychelles Time" "SCT" "+04")
    ("Singapore Time" "SGT" "+08")
    ("Sri Lanka Time" "SLT" "+05:30")
    ("Samoa Standard Time" "SST" "-11")
    ("Singapore Standard Time" "SST" "+08")
    ("Tahiti Time" "TAHT" "-10")
    ("Thailand Standard Time" "THA" "+07")
    ("Coordinated Universal Time" "UTC" "")
    ("Uruguay Standard Time" "UYT" "-03")
    ("Venezuelan Standard Time" "VET" "-04:30")
    ("Vladivostok Time" "VLAT" "+10")
    ("West Africa Time" "WAT" "+01")
    ("Western Standard Time" "WST" "+08")
    ))