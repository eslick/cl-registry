(in-package :registry)

;; Generate e-mail for:
;; - Blog posts
;; - Forum posts
;; - Survey changes
;; - New surveys
;; - "Featured visualization" (inline?)

;; =============================================================
;;  Event-based on-demand e-mail notifications
;; =============================================================

;; Utilities

(defmacro when-email-notification-enabled-p ((&optional (user-class ':users)) &body body)
  `(let ((*enable-email-to-users-default-class* ',user-class))
     (declare (special *enable-email-to-users-default-class*))
     (when (get-site-config-param :email-notification-enabled-p)
       ,@body)))

(defun send-unsubscribable-email (user subject body type)
  (send-email-to-users
   user subject
   (add-unsubscribe-link body (username user) type)
   :type type))

;; Event handlers

(defun handle-new-blog-entry (event)
  "Send blog entries to anyone who requests updates"
  (when-email-notification-enabled-p ()
    (dolist (user (users-for-preference-value :update-subscriber t))
      (multiple-value-bind (subject body)
	  (generate-blog-update-email user (event-data event))
	(send-unsubscribable-email user subject body :updates)))))

(defun handle-new-forum-post (event)
  "Notify all participants in a topic of new posts therein."
  (when-email-notification-enabled-p ()
    (let ((post (event-data event))
	  (editors (content-editors)))
      (dolist (user (union (select-if 'send-immediate-forum-update-p
				      (topic-participants (post-topic post)))
			   editors))
	(multiple-value-bind (subject body)
	    (generate-forum-post-email user post)
	  (if (member user editors)
	      (send-email-to-users user subject body :type :forums)
	      (send-unsubscribable-email user subject body :forums))))
      (multiple-value-bind (subject body)
	  (generate-forum-post-email nil post)
	(send-email '("LAMsightHelp@lamtreatmentalliance.org") subject body)))))


(defun send-immediate-forum-update-p (user)
  (and 
   (has-preference-value-p user :forum-subscriber t)
   (has-preference-value-p user :forum-email-frequency "daily")))

(defun handle-new-comment (event)
  "Send comment posts to owners"
  (when-email-notification-enabled-p (:owners)
    (let* ((comment (event-data event))
	   (question (comment-target comment))
	   (surveys (find-group-surveys (parent question)))
	   (owners (mappend #'survey-editors surveys)))
      (assert (eq (type-of question) 'question))
      (dolist (user owners)
	(multiple-value-bind (subject body)
	    (generate-survey-comment-email user comment)
	  (send-email-to-users user subject body)))
      (multiple-value-bind (subject body)
	  (generate-survey-comment-email nil comment)
	(send-email '("LAMsightHelp@lamtreatmentalliance.org") subject body)))))

(eval-when (:compile-toplevel :load-toplevel)
  (add-event-handler :new-forum-post 'handle-new-forum-post)
  (add-event-handler :new-comment    'handle-new-comment)
  (add-event-handler :new-blog-entry 'handle-new-blog-entry))


;; =============================================================
;; Timer-based Summary Digests
;; =============================================================

(define-system-event-hook send-email-digests (system-timer)
  (when (get-site-config-param :email-notification-enabled-p)
    (send-daily-digest)
    (send-weekly-digest)
    (send-monthly-digest)))

;;
;; Daily digest
;;

;; Site updates and reminders are sent out no more than weekly, so we only
;; send out forum updates here.
(defun send-daily-digest ()
  (when (send-daily-digest-p)
    (let ((since (aif (find-last-event :daily-digest)
		      (event-time it)
		      0)))
      (record-event :daily-digest nil :user nil)
      ;; Posts that occur while this loop is running will be reported
      ;; again on the next daily digest.
      (flet ((interested-p (user)
	       (and (get-preference :forum-subscriber user)
		    (equal (get-preference :forum-email-frequency user)
			   :daily))))
	(dolist (user (remove-if-not #'interested-p (all-users)))
	  ;; We only pass the user to GENERATE-FORUM-ACTIVITY-EMAIL so that we
	  ;; can use the user's language.  It would be possible to
	  ;; re-arrange things so that we could cache the generated
	  ;; subject and body in a hash table keyed on language rather
	  ;; than computing recent forum activity for each user.
          ;; If you do that, move addition of the unsubscribe link to here.
	  (multiple-value-bind (subject body)
	      (generate-forum-activity-email user since)
            (when subject
              (send-unsubscribable-email user subject body :updates))))))))

(defun send-daily-digest-p ()
  (let ((last (find-last-event :daily-digest)))
    (or (null last)
	(< (event-time last) (- (get-universal-time) *seconds-in-day*)))))

;;
;; Weekly digest
;;    

(defun send-weekly-digest ()
  (when (send-weekly-digest-p)
    (let ((since (aif (find-last-event :weekly-digest)
		      (event-time it)
		      0)))
      (record-event :weekly-digest nil :user nil)
      (labels ((wants-updates-p (user)
		 (and (get-preference :update-subscriber user)
		      (equal (get-preference :update-email-frequency user)
			     :weekly)))
	       (wants-forums-p (user)
		 (and (get-preference :forum-subscriber user)
		      (equal (get-preference :forum-email-frequency user)
			     :weekly)))
	       (wants-both-p (user)
		 (and (wants-updates-p user) (wants-forums-p user))))
	(dolist (user (all-users))
	  (cond ((wants-both-p user)
		 ;; If the user wants both, it might be nice to send only
		 ;; one message that includes everything.
		 (multiple-value-bind (subject body)
		     (generate-site-update-email user since)
		   (send-unsubscribable-email user subject body :updates))
		 (multiple-value-bind (subject body)
		     (generate-forum-activity-email user since)
		   (send-unsubscribable-email user subject body :forums)))
		((wants-updates-p user)
		 (multiple-value-bind (subject body)
		     (generate-site-update-email user since)
		   (send-unsubscribable-email user subject body :updates)))
		((wants-forums-p user)
		 (multiple-value-bind (subject body)
		     (generate-forum-activity-email user since)
		   (send-unsubscribable-email user subject body :forums)))))))))

;; MOTD
;; Suggested next steps
;; Update/changed surveys you've taken
;; New forum posts (links & summary)

(defun send-weekly-digest-p ()
  (aif (find-last-event :weekly-digest)
       (< (event-time it) (- (get-universal-time) *seconds-in-week*))
       t))

;;
;; Monthly digest
;;

(defun send-monthly-digest ()
  (when (send-monthly-digest-p)
    (let ((since (event-time (find-last-event :monthly-digest))))
      (record-event :monthly-digest nil :user nil)
      (labels ((wants-updates-p (user)
		 (and (get-preference :update-subscriber user)
		      (equal (get-preference :update-email-frequency user)
			     :monthly)))
	       (wants-forums-p (user)
		 (and (get-preference :forum-subscriber user)
		      (equal (get-preference :forum-email-frequency user)
			     :monthly)))
	       (wants-both-p (user)
		 (and (wants-updates-p user) (wants-forums-p user))))
	(dolist (user (all-users))
	  (cond ((wants-both-p user)
		 ;; If the user wants both, it might be nice to send only
		 ;; one message that includes everything.
		 (multiple-value-bind (subject body)
		     (generate-site-update-email user since)
		   (send-unsubscribable-email user subject body :updates))
		 (multiple-value-bind (subject body)
		     (generate-forum-activity-email user since)
		   (send-unsubscribable-email user subject body :forums)))
		((wants-updates-p user)
		 (multiple-value-bind (subject body)
		     (generate-site-update-email user since)
		   (send-unsubscribable-email user subject body :updates)))
		((wants-forums-p user)
		 (multiple-value-bind (subject body)
		     (generate-forum-activity-email user since)
		   (send-unsubscribable-email user subject body :forums)))))))))

(defun send-monthly-digest-p ()
  (let ((last (find-last-event :monthly-digest)))
    (or (null last)
	(< (event-time last) (- (get-universal-time) *seconds-in-month*)))))
