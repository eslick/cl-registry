(in-package :registry)

(registry-proclamations)

(defparameter *lazy-fulltext-indexing* nil
  "Inhibit indexing on object creation and do it explicitly
   via index-object")

;;
;; Support automatic indexing of model classes
;;

(defpclass fulltext-mixin ()
  ())

(defgeneric fulltext-fields (instance)
  (:documentation "A generic function to be implemented by inheriting classes
   to define the fields to be indexed")
  (:method ((instance t)) nil))

(defmethod initialize-instance :after ((inst fulltext-mixin) &rest args)
  (declare (ignore args))
  (unless *lazy-fulltext-indexing*
    (index-object inst)))

(defmethod fulltext-document ((inst fulltext-mixin))
  (unless (fulltext-fields inst)
    (error "Field list not defined for class ~A inheriting from fulltext-mixin!"
	   (class-of inst)))
  (auto-create-fulltext-document inst #'object-id
				 (mapcar #'mklist (fulltext-fields inst))))
				 
  
(defmethod drop-instance :before ((inst fulltext-mixin))
  (ignore-errors 
    (unindex-object inst)))

(defmethod (setf closer-mop:slot-value-using-class)
    (value (obj fulltext-mixin) instance slot-def)
  "Make sure if we change the object value, we update the index!"
  (declare (ignore instance value))
;;  (log-message :fulltext :debug "Updating fulltext index for: ~A" instance)
  (let ((slotname (closer-mop:slot-definition-name slot-def)))
    (when (member slotname (fulltext-fields obj))
      (montezuma:update *fulltext-index* (object-id obj) 
			`((,(symbol-name slotname) . value))))))
  

  
