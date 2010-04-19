(in-package :registry)

(registry-proclamations)

;;; We need to keep track of certain bits of global state.
;;; Do it with this object.

(defpclass forums-misc-data ()
  ((current-topic-number :initform 0)))

(defvar *forums-misc-data* nil)

(define-system-event-hook forums-startup (start-app)
  (let ((data (get-from-root '*forums-misc-data*)))
    (unless data
      (setq data (make-instance 'forums-misc-data))
      (add-to-root '*forums-misc-data* data))
    (setq *forums-misc-data* data)))

;;; We number forum topics sequentially.  When creating a new topic,
;;; we get the next number by calling this function.
(defun next-topic-number ()
  (with-slots (current-topic-number) *forums-misc-data*
    ;; thread safe?
    (ensure-transaction ()
      (incf current-topic-number))))

;; ====================================
;; Categories
;; ====================================

(defparameter *forum-category-per-center-p* nil
  "True if forum categories should be per center.")

(defun forum-category-center ()
  (and *forum-category-per-center-p* (current-center)))

(defmodel forum-category (user-translation-mixin)
  ((name :accessor category-name :initarg :name)
   (short :accessor category-short-description :initarg :short-description :initform "")
   (center :accessor center :initarg :center :initform (forum-category-center)
           :index t)
   (post-permissions :accessor category-permissions :initarg :permissions :initform nil))
  (:documentation "The forums category object maintains a top level set of
    forum areas that the widgets use to split aparts kinds of postings and
    provide a little more hierarchical organization"))

(defmethod translate-fields ((obj forum-category))
  '(name))

;; for admin

(defview forum-category-table-view (:type table :inherit-from '(:scaffold forum-category))
  (post-permissions :hidep t))

(defview forum-category-data-view (:type data :inherit-from '(:scaffold forum-category))
  (post-permissions :hidep t))

(defview forum-category-form-view (:type form :inherit-from '(:scaffold forum-category))
  (post-permissions :hidep t))

;; Useful helpers

(defun all-categories (arg)
  (declare (ignore arg))
  (find-persistent-objects *registry-main* 'forum-category))

(defun default-category ()
  (first (all-categories t)))

;; ====================================
;; Topics
;; ====================================

(defmodel forum-topic (user-translation-mixin rateable-mixin)
  (;; metadata
   (current-post-number :initform 0)
   (number :accessor topic-number :initarg :number)
   (category :accessor topic-category :initarg :category :type forum-category
	     :initform (default-category) :index t)
   (owner :accessor topic-owner :initarg :owner :initform (current-user)
	  :type user)
   (tags :accessor topic-tags :initarg :tags :type list)
   (slug :accessor topic-slug :initarg :slug :initform "" :type string)
   (subject :accessor topic-subject :initarg :subject :type string :initform "")
   (content :accessor topic-content :initarg :content :type string :initform "")
   (content-type :accessor topic-content-type :initarg :content-type :initform nil)
   (related-content :accessor topic-related-content :initarg :related-content
		    :initform nil)
   (date-created :accessor topic-date-created :initarg :date-created 
		 :initform (get-universal-time) #| :type datetime |#)
   (date-updated :accessor topic-date-updated :initarg :date-update 
		 :initform (get-universal-time) #| :type datetime |#))
   ;; content
  (:documentation "Forum topics are observations or questions that people can
   discuss by adding additional postings.  The root post is usually formatted
   specially as a topic for the thread as well as the description"))

(defparameter *preserved-html*
  '("<!--"                              ;comment
    "<br>"                              ;break
    ))

;; clean-markdown-html turns "<" chars into "&lt;" unless they begin
;; a string that is in *preserved-html*.
(defun clean-markdown-html (string)
  (with-output-to-string (s)
    (flet ((begins-p (str pos len)
             (loop
                with max-j = (+ pos len)
                for i from 0 below (length str)
                for j from pos
                do
                (when (>= j max-j) (return nil))
                (unless (char-equal (elt string j) (elt str i))
                  (return nil))
                finally (return t))))
      (loop
         with pos = 0
         with len = (length string)
         for less-pos = (position #\< string :start pos)
         while less-pos
         do
           (write-string string s :start pos :end less-pos)
           (unless (dolist (str *preserved-html*)
                     (when (begins-p str less-pos len)
                       (let ((strlen (length str)))
                         (write-string string s
                                       :start less-pos
                                       :end (+ less-pos strlen))
                         (setf pos (+ less-pos strlen))
                         (return t))))
             (write-string "&lt;" s)
             (setf pos (1+ less-pos)))
         finally
           (write-string string s :start pos)))))

(defun maybe-markdown (string type)
  (if (eq type :markdown)
      (nth-value 1 (markdown:markdown (clean-markdown-html string)
                                      :stream nil :format :html))
      string))

(defmethod content-html ((topic forum-topic))
  (maybe-markdown (topic-content topic) (topic-content-type topic)))

(defmethod translate-fields ((obj forum-topic))
  '(subject content))

(defun next-post-number (topic)
  (with-slots (current-post-number) topic
    ;; thread safe?
    (ensure-transaction ()
      (incf current-post-number))))

(defun topic-participants (topic)
  "Return list of users who have posted messages in TOPIC"
  (let* ((posts (get-instances-by-value 'forum-post 'topic topic))
	 (users (remove-duplicates (mapcar #'post-owner posts))))
    (adjoin (topic-owner topic) users)))

(defview forum-topic-table-view (:type table)
  (category :reader (compose #'category-name #'topic-category))
  (owner :reader (compose #'username #'topic-owner))
  current-post-number)

(defview forum-topic-data-view (:type data :inherit-from '(:scaffold forum-topic))
  )

(defview forum-topic-form-view (:type form :inherit-from '(:scaffold forum-topic))
  (category :present-as (dropdown :choices #'all-categories
				  :label-key #'category-name)
	    :parse-as (mid)
	    :reader (compose #'mid #'topic-category)
	    :requiredp t)
  (owner :present-as (dropdown :choices #'(lambda (x) 
					    (declare (ignore x))
					    (all-users))
			       :label-key #'username)
	 :parse-as (mid)
	 :reader (compose #'mid #'topic-owner)
	 :requiredp t)
  (content :present-as (textarea))
  (tags :hidep t)
  (date-created :hidep t)
  (date-updated :hidep t)
  (number :parse-as (integer))
  (current-post-number :parse-as (integer))
  (related-content :hidep t))

;;; user-facing topic views

(defview forum-topic-add-view (:type form)
  (subject :requiredp t)
  (content :requiredp t
	   :present-as (textarea)))

;; ====================================
;; Posts
;; ====================================

(defmodel forum-post (user-translation-mixin)
  ((owner :accessor post-owner :initarg :owner) ;; :type user)
   (number :accessor post-number :initarg :number)
   (post-datetime :accessor post-datetime :initarg :datetime 
		  :initform (get-universal-time))
		  ;; :type datetime 
;;		  :initform (the datetime (get-universal-time)))
   (topic :accessor post-topic :initarg :topic :index t) ;; :type forum-topic)
   (rating :accessor post-rating :initarg :rating :initform nil)
   (title :accessor post-title :initarg :title)
   (content :accessor post-content :initarg :content)
   (content-type :accessor post-content-type :initarg :content-type :initform nil))
  (:documentation "Forum posts, for now, are linear comments with titles
   that users can post in response to prior posts and the primary topic."))

(defmethod content-html ((post forum-post))
  (maybe-markdown (post-content post) (post-content-type post)))

(defmethod translate-fields ((obj forum-post))
  '(title content))

;; Default / admin views

(defun careful-post-owner-username (post)
  (let ((owner (post-owner post)))
    (when owner
      (username owner))))

(defview forum-post-table-view (:type table :inherit-from '(:scaffold forum-post))
  (owner :reader #'careful-post-owner-username)
  (topic :hidep nil :reader (compose #'topic-subject #'post-topic))
  (content :hidep t))

(defview forum-post-data-view (:type data :inherit-from '(:scaffold forum-post))
  (owner :reader #'careful-post-owner-username)
  (title :present-as (paragraph))
#|  (rating :present-as (stars)) |#
  (content :present-as (paragraph)
           :reader 'content-html))

(defview forum-post-form-view (:type form :inherit-from '(:scaffold forum-post))
  (owner :present-as (dropdown :choices #'(lambda (x) 
					    (declare (ignore x))
					    (all-users))
			       :label-key #'username)
	 :parse-as (mid)
	 :reader (compose #'mid #'post-owner)
	 :requiredp t)
  (topic :present-as (dropdown :choices #'(lambda (arg) 
					    (declare (ignore arg))
					    (get-instances-by-class 'forum-topic))
			       :label-key #'topic-subject)
	 :parse-as (mid)
	 :reader (compose #'mid #'post-topic)
	 :requiredp t)
#|  (post-datetime :present-as (datetime)
		 :parse-as (datetime)) |#
  (number :parse-as (integer))
  (rating :parse-as (integer))
  (content :present-as (textarea)))

;; User-facing views

(defun post-prefix-fn (view obj &rest args)
  (declare (ignore args obj view))
  (with-html
    (:p "from prefix fn")))

(defun post-suffix-fn (view obj &rest args)
  (declare (ignore args view))
  (when (eq (current-user) (post-owner obj))
    (with-html
      (:p "you are the owner, and should be able to edit this post.")))
  (with-html
    (:p "from suffix fn")))

(defview forum-post-normal-view (:type post
				 :default-fields-prefix-fn 'post-prefix-fn
				 :default-fields-suffix-fn 'post-suffix-fn)
#|  (post-datetime :present-as (datetime)) |#
  (content :present-as (paragraph) :reader 'content-html)
#|  (rating :present-as (stars)) |#)

(defview forum-post-edit-view (:type form)
  title
  (content :present-as (textarea)))
