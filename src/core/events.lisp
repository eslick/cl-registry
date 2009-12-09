(in-package :registry)

;; =============================================================================
;; Event Subsystem
;; =============================================================================

;; This file contains the event subsystem.  As opposed to hooks, which deal with 
;; site administration issues (initializing libraries, loading data, configuring
;; site-specific parameters) this supports logging and responding to user and
;; system events such as new-posts, new-registrations, etc.
;;
;; The event subsystem has:
;; 1) An event persistent class and common operations over them
;; 2) The ability to register handlers that are triggered when a specific event occurs
;; 3) A timer thread which calls some hard-coded functions every hour
;;    NOTE: This should be generalized so plugins and other modules can 'hook into' it

(defparameter *beginning-of-time* 3420838940) ;; May 26, 2008

(defmethod event-time (time)
  "It is important for empty databases to have a default 'zero' time"
  (when (null time)
    *beginning-of-time*))

(defpclass site-event ()
  ((type :accessor event-type :initarg :type)
   (time :accessor event-time :initarg :time :initform (get-universal-time) 
	 :index t)
   (user :accessor event-user :initarg :user :initform nil)
   (data :accessor event-data :initarg :data :initform nil)))

(defmethod print-object ((event site-event) stream)
  (format stream "#<SITE-EVENT ~A ~A>"
	  (event-type event)
	  (event-time event)))

;;
;; Operations to manipulate events
;;
;; Record, various mapping operators, find, events-since

(defun record-event (type data &key 
		   (user (current-user)) 
		   (time (get-universal-time)))
  (bordeaux-threads:make-thread
   (lambda () (record-event-aux
	       type data user time))
   :name "event-handler"))

(defvar *event-handler-lock* nil)
  
(defun record-event-aux (type data user time)
  (unless *event-handler-lock*
    (setf *event-handler-lock* 
	  (bordeaux-threads:make-lock "event-handler")))
  (bordeaux-threads:with-lock-held (*event-handler-lock*)
    (with-return-it (make-instance 'site-event
				   :type type 
				   :data data 
				   :user user 
				   :time (or time (get-universal-time)))
      (trigger-event type it))))


(defun map-events (fn start-time &rest args)
  "Map all events since start time"
  (apply 'map-inverted-index 
	 (lambda (time e)
	   (declare (ignore time))
	   (funcall fn e))
	 'site-event 'time 
	 :start start-time args))

(defun map-events-by-type (fn type start-time &rest args)
  "Map events from time start-time filtering all but type"
  (apply 'map-events
	 (lambda (event)
	   (when (eq (event-type event) type)
	     (funcall fn event)))
	 start-time args))

(defparameter *seconds-in-day* (* 24 60 60))
(defparameter *seconds-in-week* (* 7 *seconds-in-day*))
(defparameter *seconds-in-month* (floor (* (/ 365.25 12) *seconds-in-day*)))

(defun map-events-last-day (fn &rest args)
  "Map over the last day's events"
  (apply 'map-events fn 
	 :start (- (get-universal-time) #.(* 24 60 60)) args))

(defun map-events-last-week (fn &rest args)
  "Map the last week's events"
  (apply 'map-events fn 
	 :start (- (get-universal-time) *seconds-in-week*) args))

(defun find-last-event (type)
  (map-inverted-index (lambda (key event)
			(declare (ignore key))
			(when (eq (event-type event) type)
			  (return-from find-last-event event)))
		      'site-event 'time
		      :from-end t))

(defun time-of-last-event (type)
  (aif (find-last-event type)
       (event-time it)
       0))

(defun events-since (type since)
  (let (events)
    (map-events-by-type (lambda (e)
			  (push e events))
			type since)
    (nreverse events)))



;; =============================================================================
;;  Triggers and Handlers
;; =============================================================================

(defvar *site-event-handlers* (make-hash-table))
  
(defun event-handler-fn (record) (cdr record))
(defun set-event-handler-fn (value record) (setf (cdr record) value))
(defsetf event-handler-fn set-event-handler-fn)

(defun add-event-handler (type fname &optional fn)
  (let ((new-record (make-event-handler-record fname fn))
	(old-record (assoc fname (get-event-handlers type))))
    (cond ((and fn old-record)
	   (setf (event-handler-fn old-record) fn))
	  ((not old-record)
	   (push new-record (gethash type *site-event-handlers*)))
	  (t old-record))))

(defun get-event-handlers (type)
  (gethash type *site-event-handlers*))

(defsetf get-event-handlers (type) (value)
  `(setf (gethash ,type *site-event-handlers*) ,value))

(defun remove-event-handler (type fname)
  (setf (get-event-handlers type)
	(remove fname (get-event-handlers type)
		:key 'event-handler-name)))

(defun trigger-event (type &optional event)
  "Fire any handlers when an event of type type is created.  Can
   also be used to fire events without creating an event record"
  (mapcar (lambda (rec) 
	    (if (event-handler-fn rec)
		(funcall (event-handler-fn rec) event)
		(funcall (event-handler-name rec) event)))
	  (get-event-handlers type)))

;; Handler utils

(defun make-event-handler-record (name fn)
  (cons name fn))

(defun event-handler-name (record) 
  (car record))



;; =============================================================================
;;  Timer thread events
;; =============================================================================

(defvar *timer-thread* nil)

(define-system-event-hook start-timer-thread (start-app)
  start-timer-thread)

(define-system-event-hook stop-timer-thread (stop-app)
  stop-timer-thread)

(defun start-timer-thread ()
  (stop-timer-thread)
  (setf *timer-thread*
	(bordeaux-threads:make-thread 
	 'registry-timer-thread
	 :name "registry-timers")))

(defun stop-timer-thread ()
  (when *timer-thread*
    (bordeaux-threads:destroy-thread *timer-thread*)
    (setf *timer-thread* nil)))

(defparameter *system-timer-interval* 3600)

(defun registry-timer-thread ()
  "Runs do-timer-updates of checks every hour"
  (loop 
     (fire-system-timer)
     (sleep *system-timer-interval*)))

(defun fire-system-timer ()
  "A static set of updates to run each hour"
  (funcall-hook :system-timer))

  




