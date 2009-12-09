(in-package :registry)

(registry-proclamations)

(defmodel message-template (user-translation-mixin)
  ((subject :accessor subject :initarg :subject)
   (body :accessor body :initarg :body)
   (event :accessor event :initarg :event
	  :index t
	  :documentation "A symbol denoting what the template is for.")))

(defmethod translate-fields ((thing message-template))
  '(subject body))

(defview message-template-table-view (:type table)
  subject
  event)

(defview message-template-data-view (:type data)
  subject
  body
  event)

(defview message-template-form-view (:type form)
  (subject :present-as (input :max-length 72))
  (body :present-as (textarea :cols 60 :rows 20))
  event)

(defun message-template-for-event (event)
  (setq event (string event))
  (get-instance-by-value 'message-template 'event event))

(defun plistp (p)
  (and (listp p)
       (evenp (length p))))

(defmethod fill-template ((template message-template) plist &optional lang)
  "Fill in TEMPLATE using the tag/value pairs provided in PLIST,
returning two values: the SUBJECT and the BODY."
  (check-type plist (satisfies plistp))
  (let* ((subject (slot-value-translation template 'subject lang))
	 (body (slot-value-translation template 'body lang)))
    (flet ((value-for-match (match tag-name &rest args)
	     (declare (ignore match args))
	     (setq tag-name (kintern (string-upcase tag-name)))
	     ;; If there's no entry in the plist for this tag,
	     ;; just replace it with the empty string.
	     (or (getf plist tag-name)
		 "")))
      ;; the regexps match alphanumeric characters plus #\_ and #\-
      ;; so that we can write stuff like <% NEXT-STEPS %>
      (setq subject (cl-ppcre:regex-replace-all "<%\\s*([\\-\\w]+)\\s*%>"
						subject
						#'value-for-match
						:simple-calls t))
      (setq body (cl-ppcre:regex-replace-all "<%\\s*([\\-\\w]+)\\s*%>"
					     body
					     #'value-for-match
					     :simple-calls t)))
    (values subject body)))

#||

Example:

(defvar *junk* (make-instance 'message-template
			      :subject "<% GREETINGS %> to you!"
			      :body "Your favorite <%ITEM%> is
now <% PERCENTAGE%>% off!  Shop at <%STORE %> today!"))

(fill-template *junk* '(:greetings "Guten Tag"
			:percentage "30"
			:store "the Laughing Ogre"
			:item "back scratcher"))
=>

"Guten Tag to you!"
"Your favorite back scratcher is
now 30% off!  Shop at the Laughing Ogre today!"

||#
