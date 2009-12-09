(in-package :registry)

(registry-proclamations)

;; ===================================================
;;  Wrapper support to Montezuma full text indexing
;; ===================================================

(defvar *fulltext-index* nil)

(defvar *fulltext-lock* nil
  "MP lock for side effecting montezuma index")

(defmacro with-fulltext-lock (&body exprs)
  `(bordeaux-threads:with-lock-held (*fulltext-lock*)
     (progn ,@exprs)))

;; ==================================================
;;  API: Open and close the main index
;; ==================================================

(defun open-registry-fulltext-index ()
  (open-fulltext-index (registry-fulltext-index-path)))

(defun close-registry-fulltext-index ()
  (close-fulltext-index))

(defun wipe-registry-fulltext-index ()
  (wipe-fulltext-index (registry-fulltext-index-path)))

(define-system-event-hook ensure-fulltext-index (start-app)
  open-registry-fulltext-index)

(define-system-event-hook ensure-fulltext-index (stop-app)
  close-registry-fulltext-index)

(defun registry-fulltext-index-path ()
  (make-pathname :directory (registry-relative-path (list "data"))
		 :name "fulltext"
		 :type "idx"))

(defun rebuild-fulltext-index ()
  (ignore-errors (close-registry-fulltext-index))
  (handler-case 
      (wipe-registry-fulltext-index)
    (error ()
      (cerror "Continue?" "Please wipe fulltext index manually before continuing")))
  (open-registry-fulltext-index)
  (with-fulltext-lock
    (map-class #'index-object 'question)
    (map-class #'index-object 'survey)))

;; ==================================================
;;  Generalized fulltext index interface  
;; ==================================================

(defun open-fulltext-index (path)
  "Assumes a single thread starting up at once"
  (when *fulltext-index*
    (warn "A fulltext index already exists; ignoring.")
    (return-from open-fulltext-index))
  (unless *fulltext-lock*
    (setf *fulltext-lock*
	  (bordeaux-threads:make-lock "montezuma")))
  (setf *fulltext-index* 
	(make-instance 'montezuma:index
		       :path path :default-search-field "*"
		       :analyzer (make-instance 'montezuma::stemming-analyzer))))

(defun wipe-fulltext-index (path)
  (close-fulltext-index)
  (delete-file path))

(defun close-fulltext-index ()
  (when *fulltext-index*
    (montezuma:close *fulltext-index*)
    (setf *fulltext-index* nil)))

;; ===================================================
;;  Simple object indexing interface - supports mixin
;; ===================================================

(defun index-object (object)
  (awhen (fulltext-document object)
    (with-fulltext-lock 
      (montezuma:add-document-to-index *fulltext-index* it))))

(defun unindex-object (object)
  (awhen (montezuma:get-document *fulltext-index* (mid object))
    (with-fulltext-lock
      (montezuma:delete-document *fulltext-index* it))))

;; Support for auto-indexing objects

(defgeneric fulltext-document (object)
  (:documentation "Allows a class to define an auto create function to
   use the simple object creation method for fulltext indexing")
  (:method ((object t))
    (error "Must define 'fulltext-document' to index object ~A" object)))

(defun auto-create-fulltext-document (inst id-fn field-descriptions &key store-values)
  "Create a montezuma document from an instance and description; assumes
   that the object can be recreated using the id and class.  Values are
   not stored unless explicitly requested in the field descriptions or 
   top-level store-values keyword argument"
  (let ((doc (make-instance 'montezuma:document)))
    (labels ((add (name value &key stored tokenized binary)
	       (montezuma:add-field doc (field name value stored tokenized binary)))
	     (field (name value stored tokenized binary)
	       (montezuma:make-field name value
				     :stored stored
				     :index (if tokenized 
						:tokenized
						:untokenized)
				     :binary-p binary)))
						
      (add "id" (format nil "~A" (funcall id-fn inst)) :stored t)
      (add "class" (canonicalize-symbol (class-name (class-of inst)))
	   :stored t)
      (dolist (field-desc field-descriptions)
	(destructuring-bind (name &key stored untokenized) field-desc
	  (when (slot-boundp inst name)
	    (add (canonicalize-symbol name)
		 (slot-value inst name)
		 :stored (or stored store-values)
		 :tokenized (not untokenized)))))
      doc)))
 

;; ==================================================
;;  API: Fulltext query interface
;; ==================================================

(defun fulltext-search (query &key (obj-fn #'return-object-by-id)
			primary-fn)
  "Basic lucene queries; returns a list of matching objects in score order"
  (let ((results nil)
	(query-obj (create-fulltext-query query)))
    (labels ((collect (id score)
	       (push (cons (funcall obj-fn 
				    (parse-integer
				     (montezuma:document-value 
				      (montezuma:get-document *fulltext-index* id) 
				      "id")))
			   score)
		     results)))
      (with-fulltext-lock
        (montezuma:search-each *fulltext-index* query-obj
                               (or primary-fn #'collect)))
      (nreverse results))))
  
(defun fulltext-ids-search (query)
  "Perform simple google-like text queries"
  (fulltext-search query :obj-fn #'identity))

(defun fulltext-class-search (query classes)
  "Perform simple google-like text queries over a set of classes"
  (select-if (lambda (pair) 
	       (member (type-of (car pair)) classes))
	     (mapcan #'(lambda (class)
			 (fulltext-search (format nil "+class:~A ~{+~A ~}"
						  (convert-class class)
						  (split-sequence:split-sequence #\space query))))
		     (mklist classes))))

(defun fulltext-field-search (query class field)
  "Search just the field of the requested class"
  (fulltext-class-search (concatenate 'string "+" (canonicalize-symbol field) 
				      ":\"" query "\"")
			 class))

;;
;; Query utilities
;;

(defparameter *analyzer* (make-instance 'montezuma:standard-analyzer))

(defun create-fulltext-query (string)
  string)

;;
;; Search utilities
;;

(defun return-object-by-id (id)
  "Reconstitute an object from its id"
  (find-persistent-object-by-id *default-store* nil id))

(defun return-object (id)
  "In case you aren't using the elephant data store - not really supported for now"
  (let ((doc (montezuma:get-document *fulltext-index* id)))
    (find-persistent-object-by-id *default-store* (montezuma:document-value doc "*class") id)))

(defun canonicalize-symbol (symbol)
  (string-downcase (symbol-name symbol)))

(defun convert-escapes (name)
  (substitute #\? #\- name))

(defun convert-class (classname)
  (convert-escapes (canonicalize-symbol classname)))
  
