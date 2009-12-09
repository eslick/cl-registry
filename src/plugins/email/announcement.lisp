(in-package :registry)

(defmodel announcement (user-translation-mixin)
  ((date :initarg :date :initform (get-universal-time)
	 :accessor announcement-date)
   (content :initarg :content :accessor announcement-content))
  (:documentation "News and featured content to be mailed to users."))

(defmethod translate-fields ((thing announcement))
  '(content))

(defview announcement-table-view (:type table)
  (date :present-as wl-datetime)
  content)

(defview announcement-data-view (:type data)
  (date :present-as wl-datetime)
  content)

(defview announcement-form-view (:type form)
  (date :parse-as wl-datetime
	:present-as wl-datetime)
  (content :present-as (textarea :cols 60 :rows 20)))

