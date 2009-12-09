(in-package :registry)

(registry-proclamations)

(defmodel blog-entry (user-translation-mixin)
  ((title :accessor blog-entry-title :initarg :title :initform "untitled")
   (author :accessor blog-entry-author :initarg :author
	   :initform (current-user))
   (content :accessor blog-entry-content :initarg :content :initarg nil)
   (date :accessor blog-entry-date :initarg :date
	 :initform (get-universal-time))))

(defmethod translate-fields ((obj blog-entry))
  '(title content))

;;; Admin views

(defview blog-entry-table-view (:type table)
  title
  (author :reader (compose #'username #'blog-entry-author))
  date)

(defview blog-entry-data-view (:type data)
  title
  author
  date
  (content :present-as paragraph))

(defview blog-entry-form-view (:type form)
  title
  (author :present-as (dropdown :choices #'(lambda (x) 
					    (declare (ignore x))
					    (all-users))
				:label-key #'username)
	  :parse-as (mid)
	  :reader (compose #'mid #'blog-entry-author))
  date
  (content :present-as textarea))

;;; User-visible views

(defview blog-entry-add-view (:type form :caption "Add entry"
			      :persistp nil)
  title
  (content :present-as textarea))

(defview blog-entry-edit-view (:type form :caption "Edit entry")
  title
  (content :present-as textarea))
