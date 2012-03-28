(in-package :registry)

;;
;; API Around Date & Time Utilities
;;
;; Convert between universal time and strings

;; RENDERERS

(defun render-date (stream utime &optional pattern)
  "Render the date as a string from universal time in the current locale"
  (assert (numberp utime))
  (cl-l10n:with-locale (user-locale (current-user t))
    (cl-l10n::format-date/gregorian-calendar 
     stream 
     (local-time:universal-to-timestamp utime)
     :pattern pattern)))

(defun render-time (stream utime &optional pattern)
  "Render the time as a string from universal time in the current locale"
  (assert (numberp utime))
  (cl-l10n:with-locale (user-locale (current-user t))
    (cl-l10n::format-time/gregorian-calendar 
     stream
     (local-time:universal-to-timestamp utime)
     :pattern pattern)))

(defun render-datetime (stream utime &key pattern (show-time-p t) (show-date-p t))
  "Render the date followed by the time to the stream"
  (when show-date-p
    (render-date stream utime pattern))
  (write-char #\Space stream)
  (when show-time-p 
    (render-time stream utime)))

(defun render-timestamp (stream utime &optional pattern)
  "Good for logfiles, etc.  Render universal time to stream as a timestamp"
  (cl-l10n:with-locale (cl-l10n:locale "en" :use-cache t)
    (cl-l10n:format-timestamp stream (local-time:universal-to-timestamp utime))))

(defmacro defun-render-as-print (print render)
  `(defun ,print (utime &rest args)
     (with-string-stream (stream)
       (apply #',render stream utime args))))

(defun-render-as-print print-date render-date)
(defun-render-as-print print-time render-time)
(defun-render-as-print print-datetime render-datetime)
(defun-render-as-print print-timestamp render-timestamp)


;; PARSERS

(defun parse-date (input &rest args)
  (declare (ignorable args))
  (let ((local-time::*DEFAULT-TIMEZONE local-time:+UTC-ZONE+))
    (let ((result 
	   (apply #'local-time:parse-timestring input
		  :date-separator #\/ :date-time-separator #\Space
		  :fail-on-error nil
		  :allow-missing-date-part nil
		  :allow-missing-time-part t
		  :offset -18000
		  nil)))
      (when result 
	(local-time:timestamp-to-universal result)))))

(defun parse-time (input &rest args)
  (declare (ignorable args))
  (let ((local-time::*DEFAULT-TIMEZONE local-time::+NONE-ZONE+))
    (let ((result 
	   (apply #'local-time:parse-timestring input
		  :date-separator #\/ :date-time-separator #\Space
		  :fail-on-error nil
		  :allow-missing-date-part t
		  :allow-missing-time-part nil 
		  :offset -18000
		  nil)))
      (when result 
	(local-time:timestamp-to-universal result)))))

;; TODO: INSURE FAIL ON ERROR
(defun parse-datetime (input &key fail-on-error &allow-other-keys)
  (let ((local-time::*DEFAULT-TIMEZONE local-time::+none-zone+))
    (let ((result 
	   (apply #'local-time:parse-timestring input 
		  :date-separator #\/ :date-time-separator #\Space
		  :fail-on-error fail-on-error
		  :offset -18000
		  nil)))
      (when result 
	(local-time:timestamp-to-universal result)))))
       

(defun parse-timestamp (input &rest args)
  (declare (ignorable args))
  (let ((local-time::*DEFAULT-TIMEZONE local-time::+NONE-ZONE+))
    (let ((result 
	   (apply #'local-time:parse-timestring input
		  :date-separator #\/ :date-time-separator #\Space
		  :fail-on-error nil
		  :allow-missing-date-part nil
		  :allow-missing-time-part nil
		  :offset -18000
		  nil)))
    (when result 
      (local-time:timestamp-to-universal result)))))
