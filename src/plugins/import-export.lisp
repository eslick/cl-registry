(in-package :registry)

(registry-proclamations)

;; ===============================
;;  Exporter
;; ===============================

(defparameter *anonymize-users* t)

(defgeneric export-slot (instance def))
(defgeneric import-value-aux (tag record))

(defun export-models-to-file (filename &key model-list (anonymize-users-p t))
  (with-open-file (stream filename :direction :output :if-exists :supersede)
    (let* ((*package* (find-package :registry)))
      (export-models stream
                     :model-list model-list
                     :anonymize-users-p anonymize-users-p))))

(defun export-models (stream &key model-list (anonymize-users-p t))
  "Export a list of models to a file for import elsewhere; 
   all data must fit in memory"
  (let ((*anonymize-users* anonymize-users-p))
    (dolist (model (or model-list (hash-keys *models*)))
      (export-model-instances stream model))))

(defun export-model-instances (stream model)
  (format stream "~s~%" `(model ,model))
  (flet ((export1 (instance)
           (format stream "~s~%" (export-instance instance))))
    (declare (dynamic-extent #'export1))
    (map-class #'export1 model)))

(defmethod export-instance (instance)
  ;; Do not be tempted to wrap a WITH-TRANSACTION around this
  ;; (though it should work with :READ-UNCOMMITTED true).
  ;; The MAP-CLASS above wraps a :READ-UNCOMMITTED transaction implicitly.
  `(,(mid instance)
     ,@(when (typep instance 'user-translation-mixin)
         (let ((translations (instance-translations instance)))
           (and translations `(:translations ,translations))))
     ,@(mapcan (curry #'export-slot instance)
               (weblocks::class-slots (class-of instance)))))

(defun instance-translations (instance)
  (let ((res nil))
    (dolist (def (valid-languages))
      (let* ((code (cdr def))
             (translation (get-translation instance code)))
          (when (and translation (translation-alist translation))
            (push `(,code ,@(translation-alist translation)) res))))
    res))

;;(defpclass user () ())

(defmethod export-instance ((instance user))
  (if (or (not *anonymize-users*)
          (equal (username instance) "eslick")
          (equal (get-preference :family-member-patient instance) "eslick"))
      (call-next-method)
      (cons (mid instance)
	    (list 'username (random-string :length 8)
		  'password "sha1$12008$56cd6f23046551b7513eccb0b138696a7156017f"
		  'first-name "Jane"
		  'last-name (format nil "Random User ~A" (mid instance))
		  'date-joined (get-universal-time)
		  'permissions nil
		  'preferences-btree
		  `(:BTREE ,(mid (user-preferences-btree instance))
			   ((:MAP-ME-P . T)
			    (:RESIDENCE-PROV . ,(get-preference :residence-prov instance))
			    (:RESIDENCE-COUNTRY 
			     . ,(get-preference :residence-country instance))
			    (:LAM-PATIENT-P 
			     . ,(get-preference :lam-patient-p instance))
			    (:CLINICIAN-P  
			     . ,(get-preference :clinician-p instance))))))))

;;;; SLOTS

(defmethod export-slot :around (instance slot-def)
  (when (slot-boundp instance (weblocks::slot-definition-name slot-def))
    (call-next-method)))

(defmethod export-slot (instance slot-def)
  (let* ((slotname (weblocks::slot-definition-name slot-def))
	 (value (slot-value instance slotname)))
    (unless (and (eq slotname 'mid)
                 (typep instance 'weblocks-model))
      (list slotname (export-value value)))))

(defmethod export-slot (instance (slot-def elephant::association-effective-slot-definition))
  "Handle associations as a special case?  Should be implicit mapping"
  (declare (ignore instance))
  nil)

(defmethod export-slot (instance (slot-def elephant::set-valued-slot-definition))
  (let ((slotname (weblocks::slot-definition-name slot-def))
	(values nil))
    (map-slot-set (lambda (value) (push value values))
		  (slot-value instance slotname))
    (list slotname (mapcar #'export-value values))))

(defmethod export-slot (instance (slot-def elephant::transient-effective-slot-definition))
  (declare (ignore instance))
  nil)


;;;; VALUES

(defgeneric export-value (value)
  (:documentation "Return a writable representation of value")
  (:method ((value t)) value))

;; non top-level pobjects

(defmethod export-value ((value persistent-object))
  `(:persistent-reference ,(type-of value) ,(mid value)
			  ,@(unless (member (type-of value) (hash-keys *models*))
				    (rest (export-instance value)))))
	
(defmethod export-value ((value structure-object))
  `(:structure ,(type-of value)
	       ,@(export-struct value)))

(defun export-struct (struct)
  (loop for item in (elephant::struct-slots-and-values struct) collect
       (export-value item)))

;; sequences

(defmethod export-value ((value cons))
  (list :cons (export-value (car value)) (export-value (cdr value))))

(defmethod export-value ((value string))
  value)

(defmethod export-value ((value array))
  (error "Exporting arrays not supported"))

;; collections

(defmethod export-value ((value hash-table))
  (list :hash 
	(hash-table-test value)
	(maphash (lambda (k v)
		   (cons (export-value k) (export-value v)))
		 value)))

(defmethod export-value ((value btree))
  (list :btree 
	(mid value)
	(map-btree (lambda (k v)
		     (cons (export-value k) 
			   (export-value v)))
		   value :collect t)))

(defmethod export-value ((value indexed-btree))
  (error "No support for indexed-btrees yet"))

(defmethod export-value ((value dup-btree))
  (error "No support for indexed-btrees yet"))


;; ===============================
;;  Importer
;; ===============================

(defvar *model-map* nil)
(defvar *object-map* nil)
(defvar *importing-model-objects* nil "bound to t during import")

(defun get-local-model (classname mid)
  (unless *model-map*
    (setf *model-map* (make-hash-table)))
  (or (gethash mid *model-map*)
      (make-local-model classname mid)))

(defun make-local-model (classname mid)
  (setf (gethash mid *model-map*) (make-instance classname :mid mid)))

(defun get-local-btree (oid)
  (unless *object-map*
    (setf *object-map* (make-hash-table)))
  (or (gethash oid *object-map*)
      (setf (gethash oid *object-map*) (make-btree))))

;;;; Top-level import

(defun import-registry-model-file (filename)
  (let ((hunchentoot::*session* *last-session*)
	(weblocks::*current-webapp* (first weblocks::*active-webapps*))
	(*importing-model-objects* t))
    (import-model-file filename)))

(defun import-model-file (filename)
  (with-open-file (stream filename)
    (let ((*package* (find-package :registry)))
      (import-models stream))))

(defun import-models (stream)
  (let ((hunchentoot::*session* *last-session*)
	(weblocks::*current-webapp* (first weblocks::*active-webapps*)))
    (declare (special hunchentoot::*session*
		      weblocks::*current-webapp*))
    (setf *model-map* nil
          *object-map* nil)
    (loop
       with classname = nil
       with eof = (list 'eof)
       while eof
       do
         (with-transaction ()
           (loop
              for i from 0 below 100
              for irec = (read stream nil eof)
              do
                (when (eq irec eof)
                  (setf eof nil)
                  (return))
                (cond ((eq (car irec) 'model) (setf classname (cadr irec)))
                      ((null classname)
                       (error "First sexp in export file not a model record"))
                      (t (import-instance (get-local-model classname (first irec)) 
                                          (rest irec)))))))
    (setf *model-map* nil
          *object-map* nil)))

;;;; Import Instances

(defun import-instance (instance record)
  (let ((class (class-of instance))
        (translations nil))
    (loop for (slotname . value) in (pairs record) do
         (if (eq slotname :translations)
             (setq translations value)
             (typecase (moptilities:get-slot-definition class slotname)
               (elephant::set-valued-slot-definition
                (mapc (lambda (entry)
                        (set-insert (import-value entry)
                                    instance slotname))
                      value))
               (null t)
               (t (setf (slot-value instance slotname) (import-value value))))))
    (loop
       for (lang . alist) in translations
       do (let ((tran (make-instance 'translation
				     :language lang
				     :original instance
				     :translations alist)))
	    (ele::maybe-persistent-sync tran))))
  (initialize-imported-instance instance)
  instance)

(defmethod initialize-imported-instance ((instance t))
  nil)

(defmethod initialize-imported-instance ((instance article))
  (ele::maybe-persistent-sync instance))

(defmethod initialize-imported-instance ((instance user))
  (handler-case
      (make-geotag instance t)
    (warning () nil)))

;;;; Import Values

(defun import-value (value)
  (cond ((atom value) value)
	((consp value)
	 (import-value-aux (first value) (rest value)))
	(t (error "Cannot import value: ~A" value))))

(defmethod import-value-aux ((tag (eql :persistent-reference)) rec)
  (destructuring-bind (classname mid &rest inst-rec) rec
    (cond ((member classname (hash-keys *models*))
           (get-local-model classname mid))
          ((eq classname 'permission)
           (make-permission (getf inst-rec 'name) (getf inst-rec 'description)))
          (t (unless (cddr rec) (error "Persistent object rec is a model: ~A" rec))
             (import-instance (make-instance classname) inst-rec)))))
			 
(defmethod import-value-aux ((tag (eql :structure)) rec)
  (destructuring-bind (type &rest svlist) rec
    (let ((inst (funcall (struct-constructor type))))
      (loop for (slot . value) in (pairs svlist) do
	    (setf (slot-value inst slot) 
		  (import-value value)))
      inst)))

(defmethod import-value-aux ((tag (eql :cons)) rec)
  (destructuring-bind (carval cdrval) rec
    (cons (import-value carval) (import-value cdrval))))

(defmethod import-value-aux ((tag (eql :btree)) rec)
  (let ((btree (get-local-btree (first rec))))
    (loop for (k . v) in (second rec) do
	 (setf (get-value (import-value k) btree) 
	       (import-value v)))
    btree))

(defmethod import-value-aux ((tag (eql :hash)) rec)
  (let ((hash (make-hash-table :test (first rec))))
    (loop for (k . v) in (rest rec) do
	  (setf (gethash (import-value k) hash)
		(import-value v)))
    hash))



;; =====================================
;; Dump a patient's answers
;; =====================================

(defun dump-patient-answers (patient stream)
  (mapc (lambda (answer)
	  (let* ((question (question answer))
		 (ptype (presentation-type-for-question question)))
	    (format stream "~D | ~A | ~A~%" 
		    (aif (question-number question) it "-")
		    (cond ((eq ptype 'date-presentation)
			   (if (keywordp (value answer))
			       (string-downcase (symbol-name (value answer)))
			       (with-output-to-string (strm)
				 (cl-l10n:print-time (value answer) 
						     :show-time nil
						     :show-date t
						     :stream strm))))
			  ((eq ptype 'date-range-presentation)
			   (format nil "[~A to ~A]"
				   (if (keywordp (car (value answer)))
				       (string-downcase (symbol-name (car (value answer))))
				       (with-output-to-string (strm)
					 (cl-l10n:print-time (car (value answer))
							     :show-time nil
							     :show-date t
							     :stream strm)))
				   (if (keywordp (cdr (value answer)))
				       (string-downcase (symbol-name (cdr (value answer))))
				       (with-output-to-string (strm)
					 (cl-l10n:print-time (cdr (value answer))
							     :show-time nil
							     :show-date t
							     :stream strm)))))
			  ((null (value answer))
			   "false")
			  ((eq (value answer) t)
			   "true")
			  (t (value answer)))
		    (question-name (question answer)))))
	(sort (get-instances-by-value 'answer 'user patient)
	      #'< :key (compose #'object-id #'question)))
  nil)

(defun save-patient-answers (patient filename)
  (let ((patient (get-patient patient)))
    (with-open-file (stream filename :direction :output :if-exists :supersede)
      (dump-patient-answers patient stream))))

		    

