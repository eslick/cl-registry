(in-package :registry)

(defun consent-to-estrogen-study ()
  (make-articles-dialog "estrogen-study-consent"))

(defun estrogen-study-intro ()
  (if (current-user t)
      (make-articles-dialog "estrogen-study-intro-private" '(:Continue :Cancel))
      (make-articles-dialog "estrogen-study-intro-public" '(:Continue :Cancel))))

(defun register-for-estrogen-study (user)
  (setf (get-preference :estrogen-study user) t)
  (setf (study-patient-consented-p (get-study "LAM Estrogen Study")
								   (get-patient-for-user user))
		'(:consent-p t))
  (record-event :estrogen-study-registration user)
  (send-email (list (get-site-config-param :estrogen-study-email))
	      "Estrogen Study Update"
	      (format nil "User '~A' just registered for the study" (username user))))

(defun/cc withdraw-from-estrogen-study (user)
  (when (eq (do-choice "Are you sure you want to withdraw from the LAM Estrogen Study?" '(:Yes :No)) :Yes)
    (let ((study (get-study "LAM Estrogen Study"))
	  (mid (mid (get-patient-for-user user))))
      (setf (get-preference :estrogen-study user) nil)
      (setf (study-patient-consented-p (get-study "LAM Estrogen Study")
				       (get-patient-for-user user))
			'(:consent-p nil))
      (remove-permission user :estrogen-study-activated)
      (send-email (list (get-site-config-param :estrogen-study-email))
		  "Estrogen Study Update"
		  (format nil "User '~A' just withdrew from the Estrogen study" (username user)))
      (redirect (weblocks::request-uri*)))))
    

(defwidget estrogen-preferences-widget (widget)
  ((presentations :accessor preference-presentations
		  :initform (build-preference-presentations))
   (prior-prefs :initform nil :accessor prior-rendered-preferences
		:affects-dirty-status-p nil)
   (refresh-on-close-p :initform nil :accessor refresh-on-close-p)))

(defmethod dependencies append ((prefs estrogen-preferences-widget))
  (list (make-local-dependency :stylesheet "preferences")
		(make-local-dependency :script "preferences")))

(defun make-estrogen-preferences-dialog ()
  (handler-bind ((error #'continue))
	(make-instance 'estrogen-preferences-widget)))

(defmethod render-widget-body ((widget estrogen-preferences-widget) &rest args)
  (declare (ignore args))
  (handler-bind ((error #'continue))
	(let ((*current-widget* widget))
	  (declare (special *current-widget*))
	  (setf (prior-rendered-preferences widget) nil)
	  (with-html
      (:div :id "preferences"
			(with-html-form (:post (preference-form-handler widget)
								   :id "preferences-form")
			  (:div :id "preferences-body"
					(:div :class "preferences-personal-info"
						  (:p "This dialog allows you to select a time of day, and home time zone, to receive e-mail reminders.  You will also need to make sure you have not checked 'never contact' in your user preferences.")
						  (present-preferences 
						   :estrogen-study-reminders-enabled-p
						   :estrogen-study-reminder-time)
						  (:div :style "margin-left:170px;font-size:75%;clear:both;"
								"Please enter time as 'hour:minute am/pm'")
						  (present-preferences 
						   :estrogen-study-reminder-zone)))
			  (:div :id "preferences-controls"
					(render-button "Foo" :class "hidden" :value #!"Foo")
					(render-translated-button "Done"))))))))

(defun/cc consent-user ()
  (if (eq (do-dialog "" (consent-to-estrogen-study)) :accept)
      (progn 
	(register-for-estrogen-study (current-user))
	(do-choice "Next, provide your mailing address so we can send you the Spirometer and Ovulation kits needed for the study." '(:Continue))
	(do-dialog #!"User Preferences" (make-preferences-page #!"personal"))
	(do-information #!"We now will ask if you want to receive regular reminders to fill out the daily diaries.  This is optional.")
	(do-dialog #!"Estrogen Study Contact Preferences" (make-estrogen-preferences-dialog))
	(do-information #!"You have been registered for the Estrogen Study.  A staff member from the LAM Treatment Alliance will contact you within the next 1-2 weeks by e-mail to discuss the logistics of the study.")
	(redirect "/dashboard/collect/study"))
      (do-information #!"We will not register you for the study at this time.  If you have any concerns please contact EstrogenStudy@lamtreatmentalliance.org or eslick@media.mit.edu with your concerns.")))


(defun/cc invalid-login ()
  (do-information #!"Invalid login.  Please register for an account on LAMsight to sign up for this study or reset your password if you have an account."))


(defun/cc need-to-register ()
  (do-information #!"Please register for an account on LAMsight to sign up for this study, then return to this link to register for the study."))


(define-permanent-action/cc estrogen-study-signup registry (&key &allow-other-keys)
  (when (eq (do-dialog "" (estrogen-study-intro)) :Continue)
    (if (or (current-user t)
	    (eq (do-choice "Are you a registered LAMsight user?" '(:Yes :No)) :Yes))
	(if (or (current-user t) (do-login-dialog))
	    (if (study-patient-consented-p (get-study "LAM Estrogen Study") 
					   (get-patient-for-user (current-user)))
		(do-information "You have already registered for this study.  Contact EstrogenStudy@lamtreatmentalliance.org if you need more information")
		(consent-user))
	    (invalid-login))
	(progn (need-to-register)
	       (redirect "/?action=register")))))


(defun estrogen-study-activated-p ()
  (has-permission-p (current-user) :estrogen-study-activated t))

(defun clear-estrogen-background-survey (user)
  (let* ((survey (get-survey "Estrogen Study Background"))
		 (questions (all-survey-questions survey))
		 (patient (get-patient-for-user user)))
    (with-transaction ()
      (loop for question in questions
		 do (mapc #'drop-instances
				  (get-user-answers question patient))))))

(defun activate-estrogen-study (user)
  (clear-estrogen-background-survey user)
  (add-permission user :estrogen-study-activated))


(defun estrogen-study-patients ()
  (let ((study (get-study "LAM Estrogen Study")))
	(select-if (lambda (user)
				 (awhen (get-patient-for-user user)
				   (study-patient-consented-p study it)))
			   (all-users))))

(defparameter *parse-time* "(\\d.*):(\\d.*) ([a|p])m")

(defun zone-offset (zone)
  (awhen (time-zone-offset zone)
    (let ((num (parse-integer it :start 1 :end 3)))
	  (if num
		  (if (eq (char it 0) #\-)
			  (- num) num)
		  -5))))

(defun try-field-extractions (time)
  (ppcre:register-groups-bind (h m ap) (*parse-time* time)
    (if (and h m)
		(list h m ap)
		(let ((hour (parse-integer time :start 0 :junk-allowed t)))
		  (if hour (list it 0 (when (ppcre:scan "pm" time) "p"))
			  nil)))))


(defun extract-target-offset (time)
  (awhen (try-field-extractions time)
   (destructuring-bind (h m pm) it
 	 (let* ((hour (parse-integer h))
 		    (hour (mod (if (and (equal pm "p") (< hour 12))
						   (+ hour 12) hour) 24))
		    (min (if m (parse-integer m) 0)))
	   (+ (* hour 3600)
		  (* min 60))))))

(defun user-offset-seconds (zone)
  (let* ((zoff (zone-offset zone))
		 (serv -5)
		 (diff (- zoff serv)))
    (when zoff
	  (mvbind (s1 m1 h1 d1 m1 y1 dow1 dst-p tz1) (get-decoded-time)
  	   (mvbind (s m h _ _ _ _ dstp-p) (decode-universal-time (get-universal-time))
         (let ((dst (if dstp-p 1 0)))
		   (+ (* (mod (+ h diff) 24) 3600)
			  (* m 60)
			  s)))))))

(defun estrogen-reminder-ready-p (user)
  (handler-case 
	  (when (get-preference :estrogen-study-reminders-enabled-p user)
		(let* ((time (get-preference :estrogen-study-reminder-time user))
			   (zone (get-preference :estrogen-study-reminder-zone user)))
		  (when (and time zone)
			(let ((target (extract-target-offset time))
				  (now (user-offset-seconds zone)))
			  (when (and target now)
;;				(format t "user: ~A targ: ~A now: ~A~%" (username user) target now)
				(when (and target now (<= (abs (- target now)) 60))
				  user))))))
	(error (c) (format t "~A~%" c))))

(defun send-estrogen-reminder (users)
  (when (> (length users) 0)
	(send-email-to-users users
					   #!"Estrogen Study Reminder"
					   (format nil "Dear Participant, 

Thank you for contributing to the LAM Estrogen Study. As requested, this is a reminder to complete your daily measurements and enter the results into the appropriate survey on LAMsight.  If you have any questions please contact us at: EstrogenStudy@LAMTreatmentAlliance.org. 

If at any time you wish to stop receiving this reminder email please update your settings by clicking on 'manage reminder preferences' from the 'View Studies' page and deselecting the 'enable reminders' box.

You can manage your reminder preferences on LAMsight at: https://www.lamsight.org/dashboard/collect/study

Thank you,
Estrogen Study Staff
~A
" (get-site-config-param :estrogen-study-email))
					   :from (get-site-config-param :estrogen-study-email))))

(define-system-event-hook estrogen-study-reminders (system-timer)
  (when (get-site-config-param :estrogen-study-enabled-p)
	(send-estrogen-reminder 
	 (select-if #'estrogen-reminder-ready-p
				(estrogen-study-patients)))))
