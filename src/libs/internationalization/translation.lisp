(in-package :registry)

(registry-proclamations)

;;
;; Translation objects
;;

(defpclass translation ()
  ((original :accessor translation-original :initarg :original :index t)
   (language :accessor translation-language :initarg :language)
   (trustedp :accessor translation-trustedp :initarg :trustedp :initform nil)
   (editedp :accessor translation-editedp :initarg :editedp :initform nil)
   (translation-alist :accessor translation-alist :initarg :translations :initform nil))
  (:documentation "Provides a list of field translations"))

(defview translation-table-view (:type table :inherit-from '(:scaffold translation))
  (translation-alist :hidep t))

(defview translation-form-view (:type form :inherit-from '(:scaffold translation))
  (original :hidep t)
  (translation-alist :hidep nil))

(defview translation-data-view (:type data :inherit-from '(:scaffold translation))
  (translation-alist :hidep nil))

(defmethod make-translation ((object user-translation-mixin) target-language)
  "Takes an alist of field names and content and returns a translation.
   If it cannot translate, it returns an empty translation and a second
   value indicating whether it was translated (t) or not (nil)"
  (let ((translation (make-instance 'translation
				    :language target-language
				    :original object))
	(original-language (original-language object))
	fields
	translated-p)
    (if (supports-translation-p *translation-service* original-language target-language)
	(loop for field in (translate-fields object) do
	     (let* ((content (ignore-errors (slot-value object field)))
		    (new-content (and (stringp content)
                                      (translate-field-text
                                       content original-language target-language))))
               (when new-content
                 (push (cons field new-content) fields)))
	     (setf translated-p t))
	(loop for field in (translate-fields object) do
	     (push (cons field "") fields)))
    (setf (translation-alist translation) fields)
    (persist-object *default-store* translation)
    (values translation translated-p)))

(defun translate-field-text (content original-language target-language)
  "Safe translation; auto-detect"
  (handler-case 
      (auto-translate *translation-service* content 
		      original-language target-language)
;;		      "" target-language)
    (simple-type-error ()
      "[There was an error using Google's translation service; you are welcome to translate this by hand]")
    (error ()
      "[There was an error using Google's translation service; you are welcome to translate this by hand]")))




   