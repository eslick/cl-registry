(in-package :registry)

;;; The generate-xxx-email functions in this file return (values subject
;;; body), where subject and body are strings.
;;; 
;;; These strings are created by filling in message templates.  See
;;; src/models/message-templates.lisp.
;;;
;;; The functions assume that the caller has determined that the user
;;; has elected to receive the message, so there's no checking for any
;;; of that here.

(defun generate-blog-update-email (user blog-entry)
  (let* ((lang (or (get-preference :default-language user) "en"))
	 (template (message-template-for-event :new-blog-entry)))
    (when (typep blog-entry 'blog-entry)
      (fill-template template
		     `(:author ,(username (blog-entry-author blog-entry))
		       :title ,(slot-value-translation blog-entry 'title lang)
		       :date ,(with-string-stream (stream)
			        (cl-l10n:format-time
				 stream (blog-entry-date blog-entry) t t
				 (cl-l10n::locale lang)))
		       :content ,(slot-value-translation blog-entry 'content lang)
		       :original ,(blog-entry-content blog-entry))
		     lang))))

(defun url-for-topic (topic)
  (format nil "http://www.lamsight.org/dashboard/discuss/topic/~d"
	  (topic-number topic)))

(defun generate-forum-post-email (user forum-post)
  (let* ((lang (or (get-preference :default-language user) "en"))
	 (translation (unless (equal (original-language forum-post) lang)
			(get-translation forum-post lang nil)))
	 (template (message-template-for-event :new-forum-post)))
    (when (typep forum-post 'forum-post)
      (fill-template template
		     `(:author ,(username (post-owner forum-post))
		       :url ,(url-for-topic (post-topic forum-post))
		       :topic ,(topic-subject (post-topic forum-post))
		       :title ""
                              #+nil     ;forum posts no longer get a title
                              ,(if translation 
				   (translation-field translation 'title lang)
				   (post-title forum-post))
		       :content ,(if translation
				     (translation-field translation 'content lang)
				     (post-content forum-post))
		       :original ,(if translation (post-content forum-post) ""))
		     lang))))

(defun generate-survey-comment-email (user comment)
  (let* ((lang (or (get-preference :default-language user) "en"))
	 (question (comment-target comment))
	 (template (message-template-for-event :new-comment)))
    (when (and (typep question 'question) template)
      (fill-template template
		     `(:author ,(username (comment-author comment))
		       :date ,(with-string-stream (stream)
			        (cl-l10n:format-time stream
						     (comment-date comment) t t
						     (cl-l10n::locale lang)))
		       :comment ,(comment-content comment)
		       :context ,(question-context-string question lang)
		       :question ,(question-prompt question))
		     lang))))

(defun question-context-string (question lang)
  (with-string-stream (stream)
    (dolist (object (reverse (rest (question-context question))))
      (typecase object
	(question (format stream "~A ~A~%" #!"Under question:" 
			  (slot-value-translation object 'prompt lang)))
	(survey-group (format stream "~A ~A~%" #!"On page:" 
			      (slot-value-translation object 'name lang)))
	(survey (format stream "~A ~A (http://www.lamsight.org~A)~%"
			#!"In survey:" 
			(slot-value-translation object 'name lang)
			(survey-object-edit-url object)))))))
	


(defun generate-forum-activity-email (user since &key (max-posts 5))
  "Produce a summary of forum activity since time SINCE."
  (let* ((lang (or (get-preference :default-language user) "en"))
	 (topics (recently-active-topics since))
	 (template (message-template-for-event :forum-activity-summary))
	 (new-message-count 0)
	 (update (make-array 1000 :element-type 'character :adjustable t
			     :fill-pointer 0)))
    (with-output-to-string (s update)
      (dolist (topic topics)
	(format s "~a (~a)~%" (topic-subject topic) (url-for-topic topic))
	(let* ((posts (recent-posts-in-topic topic since))
	       (n (length posts))
	       (more nil))
	  (incf new-message-count n)
	  (when (> n max-posts)
	    (setq posts (subseq posts 0 max-posts))
	    (setq more (- n max-posts)))
	  (dolist (post posts)
	    (format s "(~a)~%"
                    ;;(post-title post)
                    (username (post-owner post))))
	  (when more
	    (format s "  and ~d more~%" more))
	  (format s "~%"))))
    (when (plusp new-message-count)
      (fill-template template
		     `(:activity ,update
		       :date ,(with-string-stream (stream)
				 (cl-l10n:format-time stream
						     (get-universal-time) t nil
						     (cl-l10n::locale lang)))
		       :since ,(with-string-stream (stream)
			         (cl-l10n:format-time stream since t t
						      (cl-l10n::locale lang)))
		       :count ,(format nil "~d" new-message-count))
		     lang))))

(defun announcements-since (since)
  (remove-if #'(lambda (a)
		 (< (announcement-date a) since))
	     (get-instances-by-class 'announcement)))

(defun generate-news (user since)
  (let* ((lang (or (get-preference :default-language user) "en"))
	 (announcements (announcements-since since)))
    (with-output-to-string (s)
      (when announcements
	(format s #!"Site Announcements:~%")
	(dolist (a announcements)
	  (format s "~a~%" (slot-value-translation a 'content lang)))))))

(defun generate-featured (user since)
  (declare (ignore since))
  (let* ((lang (or (get-preference :default-language user) "en"))
	 (features nil))
    (declare (ignore lang))
    (with-output-to-string (s)
      (when features
	(format s #!"New Features:~%")))))

;;; This can be edited to return only the surveys that we really want
;;; everyone to fill out.
(defun essential-surveys ()
  (delete nil
          (mapcar #'(lambda (name)
                      (get-instance-by-value 'survey 'name name))
                  '("LAM Diagnosis" "General Medical History"
                    "Behavior and Environment" "Reproductive Health"))))

(defun incomplete-surveys (user)
  (let* ((answers (get-instances-by-value 'answer 'user user)))
    (labels ((group-complete-p (group)
	       "Return T if all questions in GROUP have answers."
	       (dolist (question (group-questions group) t)
		 (let* ((answer (find question answers :key #'question)))
		   (when (and ;;(question-required-p question)
			        (null answer))
		     (return-from group-complete-p nil))
		   ;; if the answer triggers an inline group, check that.
		   (dolist (rule (group-rules group))
		     (if (and (eq (group-rule-question rule) question)
			      (equal (group-rule-value rule) (value answer)))
		       (unless (group-complete-p (group-rule-target rule))
			 (return-from group-complete-p nil)))))))
	     (survey-complete-p (survey)
	       "Return T if every question in SURVEY is answered."
	       (dolist (group (survey-groups survey) t)
		 (unless (group-complete-p group)
		   (return-from survey-complete-p nil))))
	     (group-answered-p (group)
	       "Return T if at least one question in GROUP has an answer."
	       (some #'(lambda (q)
			 (find q answers :key #'question))
		     (group-questions group)))
	     (survey-answered-p (survey)
	       "Return T if every group in SURVEY has an answered question."
	       (dolist (group (survey-groups survey) t)
		 (unless (group-answered-p group)
		   (return-from survey-answered-p nil)))))
      (let (result)
	(dolist (s (essential-surveys) result)
	  (unless (survey-answered-p s)
	    (push s result)))
	(nreverse result)))))

(defun generate-next-steps (user &optional single)
  "Produce an ordered list of surveys that the user needs to fill out.
See http://lamsight.media.mit.edu/trac/ticket/46"
  (let* ((surveys (incomplete-surveys user)))
    (with-output-to-string (s)
      (when surveys
	(format s #!"Next survey to work on:~%")
	(dolist (survey (if single (mklist (first surveys)) surveys))
	  (format s "~a (https://www.lamsight.org~a)~%" (name survey)
		  (survey-object-view-url survey)))))))

(defun generate-forum-activity (user since)
  "Return a string containing a brief report of forum activity since SINCE."
  (format nil #!"Recent Forum Activity:~%~%~a"
          (aif-ret (nth-value 1 (generate-forum-activity-email user since))
            #!"No relevant posts since your last update")))

(defun generate-site-update-email (user since)
  (let* ((lang (or (get-preference :default-language user) "en"))
	 (template (message-template-for-event :site-update)))
    (fill-template template
                   `(:news ,(generate-news user since)
                           :featured ,(generate-featured user since)
                           :next-steps ,(generate-next-steps user t)
                           :forum-activity ,(generate-forum-activity user since))
                   lang)))

;; ============================================================
;;  Unsubscribe links
;; ============================================================

(defparameter *unsubscribe-salt*
  (ironclad:ascii-string-to-byte-array "d9b67c7703"))

;; (multiple-value-bind (salt hash) (unsubscribe-salt-and-hash username)
;;   (assert (equal hash (unsubscribe-salt-xor-hash username salt))))
(defun unsubscribe-salt-and-hash (username)
  (let ((salt (make-salt 10)))
    (values salt
            (unsubscribe-salt-xor-hash username salt))))

(defun unsubscribe-salt-xor-hash (username salt)
  (let ((hash (ironclad:digest-sequence
               :sha1
               (map 'array #'logxor
                    (ironclad:digest-sequence
                     :sha1
                     (ironclad:ascii-string-to-byte-array salt))
                    *unsubscribe-salt*
                    (ironclad:digest-sequence
                     :sha1
                     (ironclad:ascii-string-to-byte-array username))))))
    ;; hash is 160 bits = 20 bytes
    (ironclad:byte-array-to-hex-string
     (map 'array #'logxor
          (subseq hash 0 5)
          (subseq hash 5 10)
          (subseq hash 10 15)
          (subseq hash 15 20)))))

(defun base-url ()
  (let ((base "http://lamsight.org")
        (port (and weblocks:*weblocks-server*
                   (hunchentoot:acceptor-port weblocks:*weblocks-server*))))
    (if (or (null port) (eql port 8080))
        base
        (format nil "~a:~d" base port))))

(defun unsubscribe-url (username type)
  (multiple-value-bind (salt hash) (unsubscribe-salt-and-hash username)
    (format nil "~a/unsubscribe/~(~a~)/~a/~a/~a"
            (base-url) type username salt hash)))

(defun add-unsubscribe-link (body username type)
  (format nil "~a~&~%~a: ~a~%"
          body
          #!"To unsubscribe"
          (unsubscribe-url username type)))


