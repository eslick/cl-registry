(in-package :registry)

(registry-proclamations)

(defmodel article (user-translation-mixin)
  ((title :initform ""
	  :accessor article-title
	  :initarg :title
	  :cached t)
   (owner :initform (current-user t)
	  :accessor article-owner
	  :initarg :owner
	  :type (or null user))
   (page :accessor article-page
	 :initform nil
	 :initarg :page)
   (order :accessor article-order
	  :initarg :order
	  :initform 1
	  :type integer
	  :index t)
   (content :initform ""
	    :accessor article-content
	    :initarg :content
	    :cached t)
   (content-type :accessor article-content-type
                 :initarg :content-type
                 :initform nil))
  (:documentation "A model of a markdown-enabled article for managing page content")
  (:default-initargs :checked-out t)
  (:cache-style :checkout))

(defmethod translated-title ((article article))
  (slot-value-translation article 'title))

(defmethod content-html ((article article))
  (maybe-markdown (article-content article) (article-content-type article)))

(defmethod translated-content-html ((article article))
  (maybe-markdown (slot-value-translation article 'content)
                  (article-content-type article)))

(defmethod translate-fields ((obj article))
  '(title content))

;; Article editing views

(defview article-table-view (:type table :inherit-from '(:scaffold article))
  (owner :reader (compose #'username #'article-owner))
  (title :present-as (paragraph)
         :reader 'translated-title)
  (content :hidep t)
  (content-type :hidep t))

(defview article-data-view (:type data :inherit-from '(:scaffold article))
  (owner :reader (compose #'username #'article-owner)))

(defview article-form-view (:type form :inherit-from '(:scaffold article))
   (owner :present-as (dropdown :choices #'(lambda (x) 
					    (declare (ignore x))
                                            (sort (users-with-permission 'editor)
                                                  #'string-lessp
                                                  :key #'username))
 			       :label-key #'username)
	  :parse-as (object-id)
	  :reader (compose #'object-id #'article-owner)
	  :requiredp t)
   (content :present-as (wmd :cols 60 :rows 20)
            :writer (lambda (value article)
                      (setf (article-content-type article) :markdown
                            (article-content article) value)))
   (content-type :hidep t))

(defview article-page-view (:type data :inherit-from '(:scaffold article))
   (page :hidep t)
   (user :hidep t)
   (order :hidep t)
   (content :present-as (paragraph)
            :reader 'translated-content-html)
   (content-type :hidep t))
