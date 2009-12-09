(in-package :registry)

;; ======================================================
;;  Google Translation Service
;; ======================================================

(defclass google-translation-service () ())

(defparameter *google-translation-pairs*
 '(("ar" "en")
   ("zh" "en")
   ("nl" "en")
   ("en" "ar")
   ("en" "zh")
   ("en" "nl")
   ("en" "fr")
   ("en" "de")
   ("en" "el")
   ("en" "it")
   ("en" "ja")
   ("en" "ko")
   ("en" "pt")
   ("en" "ru")
   ("en" "es")
   ("de" "en")
   ("de" "fr")
   ("el" "en")
   ("it" "en")
   ("ja" "en")
   ("ko" "en")
   ("pt" "en")
   ("ru" "en")
   ("es" "en")))

(defmethod supports-translation-p ((svc google-translation-service) src target)
  (declare (ignore src target))
  t)
;;  (member (list src target) *google-translation-pairs* :test #'equal))

(defparameter *max-content-length* 400)

(defparameter *translation-service* nil
  "Global variable for site translation service")

;;
;; Perform translation via Google
;;

(defmethod auto-translate ((service google-translation-service)
			   content original-lang target-lang)
  (apply #'concatenate 'string
	 (remove-nulls 
	  (mapcar #'(lambda (segment)
		      (when (car segment)
			(let ((result (auto-translate-aux service
							  (car segment)
							  original-lang
							  target-lang)))
			  (format nil "~A~A" 
				  (or result 
				      (if (> (length (car segment)) 2)
					  (format nil "[~A]" (car segment))
					  ""))
				  (or (cdr segment) "")))))
		  (split-text content *max-content-length*)))))

(defmethod auto-translate-aux ((service google-translation-service) content original-lang target-lang)
  (handler-case
      (parse-google-translation-response
       (drakma:http-request 
	(make-translation-uri content original-lang target-lang)
	:external-format-out :utf8
	:external-format-in :utf8))
    (error () nil)))

(defun make-translation-uri (content original-lang target-lang)
  (format nil "~@[~A~]~:*~:[~;&~]~A"
	  "http://ajax.googleapis.com/ajax/services/language/translate?"
	  (drakma::alist-to-url-encoded-string
	   `(("v" . "1.0")
	     ("q" . ,content)
	     ("langpair" . ,(format nil "~A|~A" original-lang target-lang)))
	   :utf8)))
		   

(defun parse-google-translation-response (json-response)
  (destructuring-bind ((rdata (trantext . translation)) &rest args)
      (json:decode-json-from-string json-response)
    (declare (ignore rdata trantext args))
    translation))

;;
;; Tokenize input and reassemble output
;;


(defun split-text (content &optional (max-length *max-content-length*))
  "Tokenize a string as a list of string-boolean pairs (s_i, b_i), so that
  no s_i is longer than *longest*, the string is the concatenation of the s_i's
  in order, and s_i is followed by a newline if b_i is true.

  Recursive component next-split:
  Given a string, return the first segment, 
  and the remainder of the string once the segment and the newline (if it exists)
  are removed."
  (if (equal (length content) 0) (return-from split-text nil))
  (let ((first-newline (position #\Newline content
				 :end (min (length content) max-length)))
	(largest-end
	 (find-good-end (subseq content 0 (min (length content) (1+ max-length))))))
;;	 (position-if (lambda (char) (member char '(#\! #\. #\? #\;)))
;;		      content :from-end t :end (min (length content) max-length))))
    (cond ((and first-newline (< first-newline (or largest-end max-length)))
;;	   (format t "Newline occurs at ~a before sentence-end at ~a.~%"
;;		   first-newline (or largest-end max-length))
	   (cons (cons (subseq content 0 first-newline) #\Newline)
		 (split-text (subseq content (1+ first-newline)))))
	  (largest-end
;;	   (format t "Sentence-end occurs at ~a before any newline.~%"
;;		   largest-end)
	   (cons (cons (subseq content 0 (1+ largest-end)) #\Space)
		 (split-text (subseq content (1+ largest-end)))))
	  ((<= (length content) max-length) (list (cons content nil)))
	  (t
	   (let ((last-space (position #\Space content :from-end t :end max-length))
		 (last-resort (- max-length 2)))
	     (if last-space
		  (cons (cons (subseq content 0 last-space) #\Space)
			(split-text (subseq content (1+ last-space))))
		  (cons (cons (subseq content 0 (1+ last-resort)) nil)
			(split-text (subseq content (1+ last-resort))))))))))

(defun clause-ending-p (char)
  (member char '(#\! #\. #\? #\;)))

(defparameter abbrev-dict '("(?i)foo(?-i)" "bar" "baz"))

(defun find-good-end (content)
  "find-good-end will not select the following as ends of sentences:
   decimal points
   periods after abbrevs. (in the dictionary)
   periods after single letters (e.g. e.g., i.e.)
   pun"
  (let* ((reversed-content (reverse content))
	 (largest-end-backindexed
	  (scan "((?<!\")|^)([\\!\\?;]|\\.(?=\\S{2,}?))"
		reversed-content)))
    (if (not largest-end-backindexed) (return-from find-good-end nil))
    (let ((largest-end
	   (- (1- (length reversed-content)) largest-end-backindexed)))
      (unless (scan "\\." (subseq content largest-end (1+ largest-end)))
	(return-from find-good-end largest-end))
      (let ((previous-word
	     (reverse (multiple-value-bind (start end)
			  (scan "^\\S+" reversed-content
				:start (1+ largest-end-backindexed))
			(subseq reversed-content start end)))))
	(unless (str-member previous-word abbrev-dict)
	  (return-from find-good-end largest-end))
	(find-good-end (subseq content 0 largest-end))))))

;;
;; Handling Abbreviations
;;
;; provide a list of abbreviations here
;; it is concatenated in order into a long regex,
;; so if you know any regex tricks, you might try something like
;; '("(?i)foo(?-i)" "bar") or even '("(?i)foo" "bar(?-i)")

(defun str-member (str lst)
  (let ((rgx (apply #'concatenate 'string
		    (mapcar #'(lambda (abbr)
				(concatenate 'string abbr "|"))
			    lst))))
    (scan (format nil "^~a$" (subseq rgx 0 (1- (length rgx)))) str)))


;;
;; Test code
;;

#|

(defparameter testr
    "Hello Tom, it's Bob, from the office down the hall. It's good to see you, buddy, how've you been?
Things have been OK for me, except that I'm a zombie now. I really wish you'd let us in.
I think I speak for all of us, when I say I understand why you folks might hesitate to submit to our demands.
But here's an FYI: You're all going to die, slowly!

All we want to do is eat your brains. We're not unreasonable; I mean, no one's going to eat your eyes.
All we want to do is eat your brains. We're at an impasse here. Maybe we should compromise.
You'll open up the doors.
We'll all come inside and eat your brains!")


(defmethod gt-test (content origin target)
  (auto-translate (make-instance 'google-translation-service)
		  content origin target))

|#