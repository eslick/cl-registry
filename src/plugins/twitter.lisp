(in-package :registry)

;;
;; Quick hack to send twitter updates
;;

(define-system-event-hook update-twitter-feed (system-timer)
  (when (twitter-enabled-p)
    (update-twitter-feed)))

(defparameter *twitter-status* nil)

(defun twitter-enabled-p ()
  (aif *twitter-status*
       (eq it :enabled)
       (aif (get-site-config-param :twitter-enabled-p)
	    (progn (twitter-enable) t)
	    (progn (twitter-disable) nil))))

(defun twitter-enable ()
  (setf *twitter-status* :enabled))

(defun twitter-disable ()
  (setf *twitter-status* :disabled))

;;
;; Authenticating user
;;

(defparameter *registry-twitter-user* nil)

(defun get-twitter-user ()
  "Unless we've created our user, get and cache it"
  (aif-ret *registry-twitter-user*
    (when (get-site-config-param :twitter-enabled-p)
      (prog1 
	  (setf *registry-twitter-user*
		(twit::get-user (get-site-config-param :twitter-username)))
	(setf (twit:twitter-user-password *registry-twitter-user*)
	      (get-site-config-param :twitter-password))))))

;;
;; Sending feeds
;;

(defun update-twitter-feed ()
  (when (send-twitter-daily-update-p)
    (multiple-value-bind (text stats)
	(lamsight-tweet-text)
      (twitter:send-tweet text :user (get-twitter-user))
      (record-event :twitter-update stats :user nil)))

  (when (send-twitter-weekly-update-p)
    (twitter:send-tweet (lamsight-weekly-tweet-text) :user (get-twitter-user))
    (record-event :twitter-weekly-update nil :user nil)))

;;
;; Generating stats and text
;;

(defmacro alist-binds (&rest terms)
  `(mapcar #'cons ',terms (list ,@terms)))

(defun lamsight-tweet-text ()
  (let* ((since (time-of-last-event :twitter-update))
	 (logins (length (events-since :user-login since)))
	 (comments (length (events-since :new-comment since)))
	 (attempts (length (events-since :registration-sent since)))
	 (registrations (length (events-since :registration-success since)))
	 (posts (length (events-since :new-forum-post since))))
    (values (format nil "Daily activity: ~A ~:*~[logins~;login~:;logins~], ~A new ~:*~[users~;user~:;users~] (~A ~:*~[attempts~;attempt~:;attempts~]), ~A ~:*~[posts~;post~:;posts~], and ~A ~:*~[comments~;comment~:;comments~]."
		    logins registrations attempts posts comments)
	    (alist-binds logins registrations attempts posts comments))))

(defun lamsight-weekly-tweet-text ()
  (let* ((users (length (all-users)))
	 (lam-patients (length (all-patients)))
	 (researchers (length (select-if #'researcher-p (all-users))))
	 (questions (length (get-instances-by-class 'question)))
	 (answers (length (get-instances-by-class 'answer)))
	 (posts (length (get-instances-by-class 'forum-post)))
	 (comments (length (get-instances-by-class 'comment))))
    (format nil "Site Statistics: ~A accounts, ~A patients, ~A researchers, ~A questions, ~A answers, ~A posts, and ~A comments."
	    users lam-patients researchers questions answers posts comments)))

(defun send-twitter-daily-update-p ()
  (aif (find-last-event :twitter-update)
       (< (event-time it) (- (get-universal-time) *seconds-in-day*))
       t))

(defun send-twitter-weekly-update-p ()
  (aif (find-last-event :twitter-weekly-update)
       (< (event-time it) (- (get-universal-time) *seconds-in-week*))
       t))

(defun show-daily-history (feature)
  "Show history of various events by day from twitter-event data"
  (map-events-by-type (lambda (e) 
			(awhen (assoc feature (event-data e) )
			  (cdr it)))
		      :twitter-update 0))
