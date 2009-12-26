;; -*- Mode:Lisp; tab-width:2; indent-tabs-mode:nil -*-

(in-package :registry)

(registry-proclamations)

(defwidget patient-editor (composite)
  ((visible-p :accessor visible-p :initform nil)))
 
(defmethod render-widget-body ((widget patient-editor) &rest args)
  (declare (ignore args))
  (let* ((user (current-user t))
         (center (current-center))
         (clinician (and user center
                         (or (has-permission-p user :admin)
                             (get-clinician user center)))))
    (when (setf (visible-p widget) (not (null clinician)))
      (with-html
        (:h2 "Add / Delete Patient")
        (:hr)))))

(defmethod render-widget-children :around ((widget patient-editor) &rest args)
  (declare (ignore args))
  (when (visible-p widget) (call-next-method)))

(defun string-or-nil-lessp (x y)
  (cond ((null x) (not (null y)))
        ((null y) nil)
        (t (string-lessp x y))))

(defun string-or-nil-greaterp (x y)
  (cond ((null x) (not (null y)))
        ((null y) t)
        (t (string-greaterp x y))))

(defun patient-editor-query (widget sort range &key countp)
  (declare (ignore widget))
  (let* ((center (current-center))
         (res (and center (get-patients-for-center center))))
    (cond (countp (length res))
          (t (destructuring-bind (col . dir) (or sort '(nil . nil))
               (unless (eq col 'id)
                 (setf res (sort res (if (eq dir :asc)
                                         #'string-lessp
                                         #'string-greaterp)
                                 :key #'id)))
               (when sort
                 (setf res (sort res
                                 (if (eq dir :asc)
                                     #'string-or-nil-lessp
                                     #'string-or-nil-greaterp)
                                 :key (cond ((eq col 'user) #'patient-username)
                                            ((eq col 'center)
                                             #'patient-center-short-name)
                                            (t #'id))))))
             (when range
               (setf res (subseq res (car range) (cdr range)))))
          res)))

(defun make-patient-editor-widget ()
  (let ((edw (make-instance 'patient-editor))
        (gred (make-instance 'gridedit
                             :name 'patient-grid
                             :data-class 'patient
                             :view 'patient-table-view
                             :item-form-view 'patient-form-view
                             :on-query 'patient-editor-query)))
    (setf (composite-widgets edw) (list gred))
    ;; Returns
    edw))

    
