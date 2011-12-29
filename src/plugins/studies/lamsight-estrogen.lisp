(in-package :registry)

(defun consent-to-estrogen-study ()
  (make-articles-dialog "estrogen-study-consent"))

(defun estrogen-study-intro ()
  (if (current-user t)
      (make-articles-dialog "estrogen-study-intro-private")
      (make-articles-dialog "estrogen-study-intro-public")))

(defun register-for-estrogen-study (user)
  (setf (get-preference :estrogen-study user) t)
  (pushnew (mid user) (patients-consented (get-study "LAM Dyspnea and Hormone Study")))
  (record-event :estrogen-study-registration :consent-accepted)
  (send-email "lamsight-admin@media.mit.edu"
	      "Estrogen Study Update"
	      (format nil "User '~A' just registered for the study" (username user))))

(define-permanent-action/cc estrogen-study-signup registry (&key &allow-other-keys)
  (estrogen-study-intro)
  (if (or (current-user t)
	  (do-choice "Are you a registered LAMsight user?" '(:Yes :No)))
      (progn
	(unless (current-user t)
	  (do-login-dialog))
	(if (eq (do-dialog "" (consent-to-estrogen-study)) :accept)
	    (progn 
	      (register-for-estrogen-study (current-user))
	      (do-information #!"You have been registered for the Estrogen Study.  A staff member from MIT or the LAM Treatment Alliance will contact you in the next 1-2 weeks by e-mail to discuss the logistics of the study"))
	    (do-information #!"We will not register you for the study at this time.  If you have any concerns please contact EstrogenStudy@lamtreatmentalliance.com or eslick@media.mit.edu with your concerns.")))
      (do-information #!"Please register on LAMsight to sign up for this study.")))

