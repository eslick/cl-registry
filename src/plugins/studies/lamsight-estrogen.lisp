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
  (send-email "EstrogenStudy@lamtreatmentalliance.org"
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
      (send-email "EstrogenStudy@lamtreatmentalliance.org"
		  "Estrogen Study Update"
		  (format nil "User '~A' just withdrew from the Estrogen study" (username user)))
      (redirect (weblocks::request-uri*)))))
    

(defun/cc consent-user ()
  (if (eq (do-dialog "" (consent-to-estrogen-study)) :accept)
      (progn 
	(register-for-estrogen-study (current-user))
	(do-choice "Next, provide your mailing address so we can send you the Spirometer and Ovulation kits needed for the study." '(:Continue))
	(do-dialog #!"User Preferences" (make-preferences-page #!"personal"))
	(do-information #!"You have been registered for the Estrogen Study.  A staff member from the LAM Treatment Alliance will contact you within the next 1-2 weeks by e-mail to discuss the logistics of the study.")
	(redirect "/dashboard/collect/study"))
      (do-information #!"We will not register you for the study at this time.  If you have any concerns please contact EstrogenStudy@lamtreatmentalliance.com or eslick@media.mit.edu with your concerns.")))


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
		(do-information "You have already registered for this study.  Contact EstrogenStudy@lamtreatmentalliance.com if you need more information")
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
	 do (drop-instances 
	     (get-user-answers question patient))))))

(defun activate-estrogen-study (user)
  (clear-estrogen-background-survey user)
  (add-permission user :estrogen-study-activated))

