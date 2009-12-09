(in-package :registry)

(registry-proclamations)

;;
;; Simple aggregate mechanism for getting elements from a parent composite
;;

(defclass peer-mixin ()
  ((peer-name :accessor peer-name :initarg :peer-name :initform nil)))

(defmethod get-peer-widget ((widget peer-mixin) name)
  (when (not (null name))
    (find name (composite-widgets (widget-parent widget)) 
	  :key #'peer-name :test #'equal)))

(defmethod find-peer-widget ((widget peer-mixin) name &optional errorp)
  (labels ((find-with-parent (parent)
	     (let ((widgets (ignore-errors (composite-widgets parent))))
	       (when widgets
		 (find name widgets :key #'peer-name :test #'equal)))))
    (let ((current widget))
      (loop for parent = (widget-parent current) do
	   (progn
	     (acond ((null parent)
		     (return (when errorp (error "No widget ~A found" name))))
		    ((find-with-parent parent)
		     (return it))
		    (t (setf current parent))))))))
		     
    


  
