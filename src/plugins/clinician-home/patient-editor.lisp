;; -*- Mode:Lisp; tab-width:2; indent-tabs-mode:nil -*-

(in-package :registry)

(registry-proclamations)

(defwidget patient-editor (composite)
  ((obj :accessor patient-editor-obj :initarg :patient :initform nil)))

(defmethod render-widget-body ((widget patient-editor) &rest args)
  (declare (ignore args))
  (with-html
    (:h2 "Add / Edit Patient")
    (:p "Current patient ID: "
        (str (format nil "~:[not set~;~:*~A~]"
                     (let ((obj (patient-editor-obj widget)))
                       (if obj (id obj))))))
    (:hr)))

(defun make-patient-editor-widget ()
  (let ((edw (make-instance 'patient-editor))
        (gred (make-instance 'gridedit
                             :name 'patient-grid
                             :data-class 'patient
                             :view 'patient-table-view
                             :item-form-view 'patient-form-view)))
    (setf (composite-widgets edw) (list gred))
    ;; Returns
    edw))

    
