(in-package :registry)

(registry-proclamations)

;;
;; Handle navigation through admin interface
;;

(define-plugin admin (site-app)
  "The create function accepts a list of plugins"
  :tab-name 'explorer
  :create 'make-admin-widget)


(defun make-admin-widget (&rest args)
  (declare (ignore args))
  (make-instance 'composite :widgets 
		 (list (make-admin-grid))))

;;
;; Specialize gridedit
;;

(defwidget admin-grid (datagrid)
  ())

(defun make-admin-grid ()
  (make-instance 'admin-grid
;;		 :name 'admin-grid
		 :data-class 'admin-model
		 :view 'admin-table-view
;;		 :item-data-view 'admin-data-view
		 :allow-drilldown-p t
;;		 :on-query 'admin-grid-query
		 :autoset-drilled-down-item-p t
		 :on-drilldown (cons :view-models #'goto-model-view)))

(defun goto-model-view (widget item)
  (with-flow widget
    (yield (make-model-view item))))

(defun admin-grid-query (admin sort range &rest args)
  (declare (ignore admin))
  (refresh-admin-models)
  (apply #'find-persistent-objects 
	 *registry-main* 'admin-model
	 :order-by sort
	 :range range
	 args))

(defun refresh-admin-models ()
  (loop for key being the hash-key of *models* do
        (unless (gethash key *registered-models*)
	  (register-model (find-class key)))))

(define-system-event-hook refresh-admin-models (start-app)
  refresh-admin-models)

;;
;; Model admin view & creation
;;

(defpclass admin-model ()
  ((name :accessor model-name :initarg :name :index t)
   (description :accessor model-description :initform "" :initarg :description)))

(defview admin-table-view (:type table :inherit-from '(:scaffold admin-model))
  (widget :hidep t))

(defview admin-data-view (:type data :inherit-from '(:scaffold admin-model))
  (widget :hidep t))


(defun register-model (model-class &optional (store *default-store*))
  (let ((amod (ensure-admin-model model-class store)))
    (setf (gethash (class-name model-class) *registered-models*) amod)
    amod))

(defun ensure-admin-model (class &optional (store *default-store*))
  (let* ((objects (find-persistent-objects store 'admin-model))
	 (model (find (class-name class) objects :key #'model-name)))
    (if model 
	(progn
	  (setf (model-description model) (documentation class t))
	  model)
	(let ((new-model (make-instance 'admin-model
					:name (class-name class)
					:description (documentation class t))))
	  (persist-object store new-model)))))

(defun unregister-model (model-name &optional (store *default-store*))
  (let ((model (gethash model-name *registered-models*)))
    (delete-persistent-object store model)
    (remhash model-name *registered-models*)))


;;
;; Model views
;;

(defun make-model-view (model &optional (answer t))
  (make-instance 'gridedit 
		 :name (model-name model)
		 :data-class (model-name model)
		 :view (model-table-view-name model)
		 :item-data-view (model-data-view-name model)
		 :item-form-view (model-form-view-name model)
		 :common-ops (when answer
			       `((done . ,#'(lambda (w i)
					      (declare (ignore i))
					      (answer w)))))))

(defmethod model-table-view-name (model)
  (intern (nstring-upcase (format nil "~A-table-view" (model-name model)))
	  (symbol-package (model-name model))))

(defmethod model-data-view-name (model)
  (intern (nstring-upcase (format nil "~A-data-view" (model-name model))) 
	  (symbol-package (model-name model))))

(defmethod model-form-view-name (model)
  (intern (nstring-upcase (format nil "~A-form-view" (model-name model))) 
	  (symbol-package (model-name model))))


