(in-package :registry)

;;
;; Article editor
;;

(defwidget content-editor ()
  ((article-grid :accessor grid :initarg :grid 
		 :initform (make-model-view 
			    (ensure-admin-model 
			     (find-class 'article))
			    nil)))
   (:documentation "An interface for content editors to update articles"))

(defmethod dependencies append ((widget content-editor))
  (wmd-dependencies))

(defmethod render-widget-body ((widget content-editor) &rest initargs)
  (declare (ignore initargs))
  (render-widget (grid widget)))

(defun make-content-widget ()
  (make-instance 'content-editor))


;;
;; Message of the month
;;

;;(defwidget motm-editor (datagrid)
;;  ((announcement-grid :accessor grid :initarg :grid
;;		      :initform (make-motm-grid)))
;;  (:documentation "A simple editing interface for the message of the month for email updates"))


