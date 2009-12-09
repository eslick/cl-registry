(in-package :registry)

(registry-proclamations)

(defmodel comment ()
  ((target :accessor comment-target :initarg :target :index t)
   (author :accessor comment-author :initarg :author :index t)
   (content :accessor comment-content :initarg :content)
   (date :accessor comment-date :initarg :date :initform (get-universal-time))
   (status :accessor comment-status :initform :open :index t))
  (:documentation "The status slot is a keyword (such as :open, :cleared,
:censored, etc.)  representing the moderation status of a comment."))

(defmethod initialize-instance :after ((comment comment) &rest initargs)
  (declare (ignore initargs)))

(defun add-comment (target content)
  (assert (and target (stringp content)))
  (with-return-it (make-instance 'comment
			       :target target
			       :content content
			       :author (current-user))
    (record-event :new-comment it)))

(defun get-comments (target)
  (get-instances-by-value 'comment 'target target))

(defun has-comments-p (target)
  (objects-exist-p 'comment 'target target))

(defun comment-count (target)
  (object-subset-count 'comment 'target target))

;;
;; Views
;;

;; Maybe this belongs in Weblocks
(defmethod form-view-field-required-text :around (field)
  (let ((res (call-next-method field)))
    (if (functionp res) (funcall res) res)))

(defview comment-add-view (:type form :persistp nil)
  (content :requiredp t
;;           :required-text (lambda () #!"required")
	   :present-as (textarea)
           :label (lambda () #!"Content")))

(defview comment-table-view (:type table)
  elephant::oid
  date
  author
  (content :present-as (paragraph))
  target)

(defview comment-data-view (:type data)
  author
  content
  target
  date
  status)

(defview comment-form-view (:type form)
  author
  content
  target
  date
  status)
