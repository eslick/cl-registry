(in-package :registry)

(defmodel guide ()
  ((title :accessor title :initarg :title :initform nil)
   (show-title-p :accessor show-title-p :initarg :show-title-p :initform t)
   (children :accessor guide-children :initarg :children :initform nil)))

(defpclass guide-node ()
  ((title :accessor title :initarg :title :initform nil)
   (children :accessor guide-children :initarg :children :initform nil)
   (body :accessor guide-body :initarg :body :initform nil)))

;;
;; Table of contents for a guide
;;

(defmethod compute-toc ((guide guide))
  (loop 
     for i from 0
     for child in (guide-children guide)
     collect
       (list i child (compute-toc child))))

(defmethod compute-toc ((guide guide-node))
  (loop 
     for i from 1
     for child in (guide-children guide)
     collect
       (list i child (compute-toc child))))


(defmethod compute-toc ((guide t))
  nil)

;;
;; Exporting and importing guide content
;;

(defun export-content (root filename)
  (with-open-file (stream filename :direction :output :if-exists :supersede)
    (write (export-object root) :stream stream)))

(defmethod export-object ((guide guide))
  `(:guide ,(title guide) ,(show-title-p guide) 
	   ,(mapcar #'export-object (guide-children guide))))
  
(defmethod export-object ((guide guide-node))
  `(:guide-node ,(title guide) ,(guide-body guide)
		,(mapcar #'export-object (guide-children guide))))


(defun import-content (filename)
  (with-open-file (stream filename)
    (let ((data (read stream)))
      (import-object (first data) data))))

(defmethod import-object ((type (eql :guide)) data) 
  (multiple-value-bind (type title show-p children) data
    (declare (ignore type))
    (make-instance 'guide 
		   :title title
		   :show-title-p show-p
		   :children (mapcar #'(lambda (rec)
					 (import-object (first rec) rec))
				     children))))


(defmethod import-object ((type (eql :guide-node)) data)
  (multiple-value-bind (type title body children) data
    (declare (ignore type))
    (make-instance 'guide 
		   :title title
		   :body body
		   :children (mapcar #'(lambda (rec)
					 (import-object (first rec) rec))
				     children))))


