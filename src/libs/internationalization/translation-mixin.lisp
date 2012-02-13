(in-package :registry)

(defparameter *translation-service* (make-instance 'google-translation-service))

(defun set-translation-service (service-object)
  (setf *translation-service* service-object))

;;
;; For models that export a UI supporting translations
;;

(defpclass user-translation-mixin ()
  ((original-language :accessor original-language :initarg :language 
		      :initform (default-language))
   (translation-cache :accessor translation-cache :initarg :translation-cache
		      :initform nil :transient t))
  (:documentation "Provides metadata on the class allowing translation
   of the appropriate fields to proceed automatically.  Also stores
   the language code for the language used in the object's fields"))

(defgeneric translate-fields (obj)
  (:documentation "Returns the list of slot names to translate.")
  (:method ((obj user-translation-mixin)) nil))

(defmethod initialize-instance :after ((obj user-translation-mixin) &rest args)
  ;; Set the default language for this object
  (declare (ignore args))
  (unless (and (slot-boundp obj 'original-language)
	       (not (null (original-language obj))))
    (setf (original-language obj) 
	  (aif-ret (and (boundp 'hunchentoot:*session*)
			(not (null hunchentoot:*session*))
			(hunchentoot::session-value 'current-language))
	    (default-language)))))

(defmethod get-translation ((model user-translation-mixin) language &optional create)
  (awhen (translation-cache model)
    (cond ((stringp it)
	   (when (and (not create) (equal it language))
	     (return-from get-translation nil)))
	  ((and (eq (type-of it) 'translation)
		(equal (translation-language it) language))
	   (return-from get-translation it))))
  (aif (find language (get-instances-by-value 'translation 'original model)
	     :key #'translation-language :test #'equal)
       (setf (translation-cache model) it)
       (if create 
	   (setf (translation-cache model)
		 (make-translation model language))
	   (progn
	     (setf (translation-cache model) language)
	     nil))))

(defmethod add-translation-fields ((model user-translation-mixin) language alist)
  "Adds an alist of (field . translation) pairs to the translations
   for an MODEL's LANGUAGE, overwriting any existing values for the same fields,
   and preserving existing values for other fields."
  (let ((translation (get-translation model language)))
    (cond ((eq (type-of translation) 'translation)
           (let ((old-alist (translation-alist translation)))
             (unless (dolist (new alist t)
                       (let ((old (assoc (car new) old-alist :test #'eq)))
                         (unless (and old (equal (cdr old) (cdr new)))
                           (return nil))))
               (let ((new-alist (copy-list old-alist)))
                 (dolist (cons alist)
                   (setf new-alist
                         (delete (car cons) new-alist :test #'eq :key #'car))
                   (push cons new-alist))
                 (setf (translation-alist translation) new-alist)))))
          (t (setf translation (make-instance 'translation
                                              :language language
                                              :original model)
                   (translation-alist translation) alist
                   ;; transaction may abort, so clear the cache
                   (translation-cache model) nil)
             (persist-object *default-store* translation)))
    (ele::maybe-persistent-sync translation)
    translation))

(defun translation-field (translation field &optional (use-original t))
  "Get the value of a translated model field from the tranlsation object,
   return the original object's field value if use-original is set to true (default)."
  (aif (assoc field (translation-alist translation))
       (cdr it)
       (when use-original
	 (ignore-errors 
	   (slot-value (translation-original translation) field)))))

(defun slot-value-translation (object slot &optional language)
  "Like slot-value but provides a translation or the original value"
  (let* ((lang (or language (default-language)))
	 (translation (get-translation object lang)))
    (aif translation
       (translation-field it slot)
       (slot-value object slot))))

(defun translated-slot-reader (slot)
  (lambda (object)
    (slot-value-translation object slot)))

(defmethod class-visible-slots-impl ((class (eql (find-class 'user-translation-mixin))) &rest args)
  (declare (ignore args))
  nil)

(defmethod drop-instance :before ((object user-translation-mixin))
  (drop-instances (get-instances-by-value 'translation 'original object)))


(defun map-translation-objects (fn)
  "Applies fn to each translatable object (subclasses of user-transation-mixin)."
  (mopu:map-subclasses
   'user-translation-mixin
   (lambda (cls)
     (mapc fn (find-persistent-objects *registry-main* cls)))))

(defun map-translatable-fields (fn)
  "For each field marked for translation over all translatable objects, calls: (fn obj field-name)"
  (map-translation-objects
   (lambda (obj)
     (dolist (field-name (translate-fields obj))
       (funcall fn obj field-name)))))

(defun collect-translatable-fields ()
  (let (result)
    (map-translatable-fields
     (lambda (obj field-name)
       (push
	(list obj field-name (slot-value obj field-name))
	result)))
    (nreverse result)))
