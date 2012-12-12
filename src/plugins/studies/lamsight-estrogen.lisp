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
;;  (do-choice
;;	"Thank you for your interest in the LAM Estrogen Study.  We have reached full enrollment and are no longer accepting new registrations.  Once the Study has concluded we will post a summary of the results on LAMsight.  We are very grateful for your willingness to participate and hope that you will continue to volunteer for future studies." '(:Continue)))

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
	(ensure-transaction ()
	  (select-if (lambda (user)
				   (has-permission-p user :estrogen-study-activated t))
			     (all-users)))))

(defun user-has-answer? (user question val)
  (let ((ans (latest-answer question (get-patient-for-user user))))
	(and ans (eq (value ans) val))))

(defun non-menopausal? (user)
  (user-has-answer? user (get-question "Are you post- or peri-menopausal") nil))

(defun has-regular-period? (user)
  (user-has-answer? user (get-question "Are your periods regular?") t))

(defun answer-distribution (question users)
  (compute-distribution
   (mapcar (lambda (u)
			 (value (latest-answer question (get-patient-for-user u))))
		   users)))

;; Generate data

(defparameter *report-questions* 
  '(("At what age did you start having a period?" "Age Started Period")
	("Are your periods regular?" "Regular?")
	("How many days is your menstrual cycle?" "Cycle Length")
	("For how many days do you bleed each month?" "Days Bleeding")
	("Do you experience any difference in your breathing or pain in your body at different parts of your monthly cycle?" "Cyclical Symptoms?")
	("Are you post- or peri-menopausal" "Menopause?")
	("At what age did you stop getting regular periods?" "Age of Menopause")
	("Have you ever been on hormone replacement therapy?" "HRT?")
	("Are you still taking birth control pills?" "On Birth Control")
	("Have you ever been pregnant?" "Ever pregnant?")
    ("Do you have children?" "Children?")
	("Have you had ovarian surgery?" "Ovarian Surgery?")
	("Have you ever had your uterus removed or other uterine surgery?" "Uterine Surgery?")
	("Have you ever used female hormones other than oral contraceptives?" "Hormones?")
	("Are you currently using over-the-counter hormone replacements or to treat post-menopausal symptoms?" "OTC HRT?")
	("Are you currently taking any prescription medications?" "On medications?")
	("Please list the prescription medications you are taking" "Med List")
	("Please rate your average level of daily stress" "Stress level")
	("Do you exercise?" "Exercise?")
	("How many glasses of alcohol do you currently consume per week?" "Alcohol/week")
	("Have you noticed an effect on your breathing from these interventions?" "CAM Success?")))

(defun question-rec (entry)
  (destructuring-bind (text label) entry
	(cons (get-question text) label)))

(defun patient-entry (question patient)
  (let ((ans (latest-answer question patient)))
	(if ans
		(let ((result (value ans)))
		  (cond ((eq result t) "yes")
				((eq result nil)"no")
				(t result)))
		nil)))

(defun patient-csv (stream users)
  (let ((patients (mapcar #'get-patient-for-user users))
        (qrecs (mapcar #'question-rec *report-questions*)))
	(princ "Username," stream)
	(loop for (question . label) in qrecs do
	     (format stream "~A~A" label ","))
	  (loop for patient in patients do
         (ensure-transaction (:read-uncommitted t :degree2 t)
		 (print (patient-username patient) stream)
		 (princ "," stream)
		 (progn 
		   (loop for (question . label) in qrecs do
				(let ((value (patient-entry question patient)))
				  (if value
					  (format stream "\"~A\"~A" (patient-entry question patient) ",")
					  (format stream "~A" ",")))))))))

(defun print-patient-csv (users)
  (with-string-stream (stream)
    (patient-csv stream users)))

(defun dump-patient-csv (filename users)
  (let ((path (merge-pathnames 
			   (make-pathname :name filename :type "csv")
			   (registry-relative-path '(".")))))
	(with-open-file (stream path :direction :output :if-exists :supersede :external-format :utf-8)
	  (patient-csv stream users))))

;; (answer-distribution (get-question "Are your periods regular?")
;;    (select-if non-menopausal? (estrogen-study-patients)))
;; REGISTRY> (estrogen-study-patients)
;; (#<USER (80) 'saramelloni'> #<USER (334) 'lucky'> #<USER (354) 'M.Luz'> #<USER (502) 'mcfoshay'> #<USER (736) 'AHL01'> #<USER (818) 'zsutherland'> #<USER (853) 'slgisme'> #<USER (2391) 'MiriS'> #<USER (2629) 'jennpeck77'> #<USER (2873) 'szaboteresa'> #<USER (2881) 'Kia'> #<USER (2893) 'tikishla'> #<USER (21912) 'jbdoty'> #<USER (22761) 'anniesek'> #<USER (24343) 'Jen'> #<USER (25906) 'Jennifer'> #<USER (26404) 'cat_walk'> #<USER (26964) 'Sumyue'> #<USER (28184) 'Charlotte smith'> #<USER (30943) 'Mountain Girl'> #<USER (36607) 'Cal'sGal'> #<USER (37708) 'rgray1007'> #<USER (38051) 'ezampino'> #<USER (38069) 'lucia.laugwitz'> #<USER (38425) 'sharonmunoz2003'> #<USER (39199) 'Marion'> #<USER (39272) 'Pepper81'> #<USER (39444) 'louwarts@hetnet.nl'> #<USER (39518) 'porkchop'> #<USER (41166) 'Anke'> #<USER (45705) 'clburke'> #<USER (46757) 'sitg'> #<USER (46800) 'ellsbells'> #<USER (47326) 'Pajarito'> #<USER (49019) 'shirlybenhur'> #<USER (49719) 'maureenkmccarthy'>)
;; REGISTRY>  (answer-distribution (get-question "Are your periods regular?")
;;                (select-if #'non-menopausal? (estrogen-study-patients)))
;; ((T . 14) (NIL . 13))
;; REGISTRY>  (answer-distribution (get-question "Are your periods regular?")
;;                (select-if (compose #'has-regular-period? #'non-menopausal? (estrogen-study-patients))))
			   

;; ; No value
;; REGISTRY>  (answer-distribution (get-question "Are your periods regular?")
;;                (select-if (compose #'has-regular-period? #'non-menopausal?) (estrogen-study-patients)))
			   

;; ; No value
;; REGISTRY>  (answer-distribution (get-question "Are your periods regular?")
;;                (select-if #'has-regular-period? (select-if #'non-menopausal? (estrogen-study-patients))))
			   
;; ((T . 14))
;; REGISTRY>  (answer-distribution (get-question "How many days is your menstrual cycle?")
;;                (select-if #'has-regular-period? (select-if #'non-menopausal? (estrogen-study-patients))))
			   
;; ((30 . 4) (28 . 4) (29 . 2) (4 . 1) (25 . 1) (27 . 1) (289 . 1))
;; REGISTRY>  (answer-distribution (get-question "How many days is your menstrual cycle?")
;;                (filter-if #'has-regular-period? (select-if #'non-menopausal? (estrogen-study-patients))))
			   
;; ((0 . 2) (28 . 2) (35 . 1) (1/2 . 1) (21 . 1) (8 . 1) (30 . 1) (27 . 1) (32 . 1) (40 . 1) (26 . 1))
;; REGISTRY>  (answer-distribution (get-question "How many days do you bleed each month?")
;;                (select-if #'has-regular-period? (select-if #'non-menopausal? (estrogen-study-patients))))
			   
;;  (answer-distribution (get-question "For how many days do you bleed each month?")
;;                (select-if #'has-regular-period? (select-if #'non-menopausal? (estrogen-study-patients))))
			   
;; ((5 . 5) (4 . 4) (6 . 2) (7 . 2) (8 . 1))
;; REGISTRY> (sort (answer-distribution (get-question "For how many days do you bleed each month?")
;;                (filter-if #'has-regular-period? (select-if #'non-menopausal? (estrogen-study-patients)))) #'< :key first)
			   

;; ; No value
;; REGISTRY> (sort (answer-distribution (get-question "For how many days do you bleed each month?")
;;                (filter-if #'has-regular-period? (select-if #'non-menopausal? (estrogen-study-patients)))) #'< :key #'first)
			   
;; ((0 . 2) (5/7 . 1) (3 . 1) (4 . 2) (5 . 4) (6 . 1) (7 . 1) (8 . 1))
;; REGISTRY> (sort (answer-distribution (get-question "For how many days do you bleed each month?")
;;                (select-if #'has-regular-period? (select-if #'non-menopausal? (estrogen-study-patients)))) #'< :key #'first)
			   
;; ((4 . 4) (5 . 5) (6 . 2) (7 . 2) (8 . 1))
;; REGISTRY> (sort (answer-distribution (get-question "How many days is your menstrual cycle?")
;;                (select-if #'has-regular-period? (select-if #'non-menopausal? (estrogen-study-patients)))) #'< :key #'first)
			   
;; ((4 . 1) (25 . 1) (27 . 1) (28 . 4) (29 . 2) (30 . 4) (289 . 1))
;; REGISTRY> (sort (answer-distribution (get-question "For how many days do you bleed each month?")
;;                (filter-if #'has-regular-period? (select-if #'non-menopausal? (estrogen-study-patients)))) #'< :key #'first)
			   
;; ((0 . 2) (5/7 . 1) (3 . 1) (4 . 2) (5 . 4) (6 . 1) (7 . 1) (8 . 1))
;; REGISTRY> (sort (answer-distribution (get-question "How many days is your menstrual cycle?")
;;                (filter-if #'has-regular-period? (select-if #'non-menopausal? (estrogen-study-patients)))) #'< :key #'first)
			   
;; ((0 . 2) (1/2 . 1) (8 . 1) (21 . 1) (26 . 1) (27 . 1) (28 . 2) (30 . 1) (32 . 1) (35 . 1) (40 . 1))
;; REGISTRY> (sort (answer-distribution (get-question "Have you had ovarian surgery?")
;;                (filter-if #'has-regular-period? (select-if #'non-menopausal? (estrogen-study-patients)))) #'< :key #'first)
			   

;; ; No value
;; REGISTRY> (answer-distribution (get-question "Have you had ovarian surgery?")
;;                (filter-if #'has-regular-period? (select-if #'non-menopausal? (estrogen-study-patients))))
			   
;; ((NIL . 10) (T . 3))
;; REGISTRY> (answer-distribution (get-question "Have you had ovarian surgery?")
;;                (select-if #'has-regular-period? (select-if #'non-menopausal? (estrogen-study-patients))))
			   
;; ((NIL . 14))
;; REGISTRY> 
;; 

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
				(when (and target now (<= (abs (- target now)) 30))
				  user))))))
	(error (c) (format t "~A~%" c))))

(defun send-estrogen-reminder (users)
  (when (> (length users) 0)
	(send-email-to-users users
					   #!"Estrogen Study Reminder"
					   (format nil "Hello,

Thank you for contributing to the LAM Estrogen Study. As requested, this is a reminder to complete your daily measurements on LAMsight:

Dyspnea Diary (https://www.lamsight.org/dashboard/collect/survey/19254/)
Daily Diary   (https://www.lamsight.org/dashboard/collect/survey/26209/)

You will be prompted to enter your username and password before you are taken to the diary.  Please don't forget to click on 'add new entry' to enter fresh data for the current day.

If at any time you wish to stop receiving this reminder email please update your settings by clicking on 'manage reminder preferences' from the 'View Studies' page (https://www.lamsight.org/dashboard/collect/study) and deselecting the 'enable reminders' box.

If you have any other questions please contact us at: EstrogenStudy@LAMTreatmentAlliance.org. 

Thank you,
Estrogen Study Staff
~A
" (get-site-config-param :estrogen-study-email))
					   :from (get-site-config-param :estrogen-study-email))))

(defparameter *reminder-count* 0)

(define-system-event-hook estrogen-study-reminders (system-timer)
  (when (zerop (mod (incf *reminder-count*) (* 60 4)))
	(format t "Estrogen Study Reminder Calls: ~A~%" *reminder-count*))
  (when (get-site-config-param :estrogen-study-enabled-p)
	(send-estrogen-reminder 
	 (select-if #'estrogen-reminder-ready-p
				(estrogen-study-patients)))))


(defun recent-answer-p (question user seconds-ago)
  (let ((answers (get-user-answers question (get-patient-for-user user)))
		(now (get-universal-time)))
	(> (length (select-if (f (a) (< (- now (entry-time a)) seconds-ago)) answers))
	   0)))
                                
(defun non-adhering-patients (seconds)
  (with-transaction (:read-uncommitted t :txn-nosync t)
    (let* ((question (get-question "FEV1 - trial 1")))
  	  (filter-if (f (user)
                   (recent-answer-p question user seconds))
                 (estrogen-study-patients)))))

(defun estrogen-study-adherence-report ()
  (with-transaction (:read-uncommitted t :txn-nosync t)
  (let ((question (get-question "FEV1 - trial 1"))
        (users (estrogen-study-patients))
		(seconds (* 3600 48)))
	(with-html
	 (:div
	  (:p (str (length (select-if (f (u) (recent-answer-p question u seconds)) users)))
		  " of " (str (length users)) " entered FEV data in the last 48 hours"))))))

(defun estrogen-study-fev-count-report ()
  (let ((question (get-question "FEV1 - trial 1")))
	(mapc (f (rec) 
            (format t "~A: ~A~%" (car rec) (cdr rec))) 
		  (sort (mapcar (f (u) 
                          (cons (username u) 
                                (length (get-user-answers question (get-patient-for-user u)))))
					    (estrogen-study-patients))
				#'> :key #'cdr))))
	
;;
;; Load estrogen study history
;;

(defun user-table (users)
  (let ((table (make-hash-table)))
	(mapc (f (u) 
            (let ((patient (get-patient-for-user (get-user u))))
			  (setf (gethash (mid patient) table) patient)))
		  users)
	table))

(defun question-table (surveys)
  (let ((table (make-hash-table)))
	(mapc (f (s)
            (let ((questions (all-survey-questions s)))
			  (loop for question in questions
				 do (setf (gethash (mid question) table) question))))
	      surveys)
	table))

(defun load-estrogen-answers (logfile)
  (let ((utable (user-table (estrogen-study-patients))) ;;(list (get-user "eslick"))))
		(qtable (question-table (mapcar #'get-survey 
										'("Estrogen Study Diary"
										  "Dyspnea Diary")))))
;;										  "Estrogen Study Background")))))
	(update-answers-from-log logfile qtable utable)))

		