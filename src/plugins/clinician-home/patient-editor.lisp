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
        (:h2 "Add / Edit Patient")
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

(defun delete-patients-confirmation-string (obj items)
  (declare (ignore obj))
  (let ((item-count (ecase (car items)
                      (:none (length (cdr items)))))
        (survey-count
         (loop
            for id in (cdr items)
            for patient = (get-object id)
            summing (length (get-instances-by-value 'answer 'user patient)))))
    (format
     nil
     #!"Delete ~d ~v[patient~:;patients~]~[~*~; and ~d survey answer~:; and ~d survey answers~]?"
     item-count (1- item-count) survey-count survey-count)))

(defun delete-patients-and-answers (widget items)
  (loop
     with current-patient = (current-patient)
     for id in (cdr items)
     for patient = (get-object id)
     do
       (with-transaction ()
         (dolist (answer (get-instances-by-value 'answer 'user patient))
           (drop-instance answer))
         (when (eq patient current-patient)
           (setf (current-patient) nil))
         (drop-instance patient)))
  (mark-dirty-sibling-widgets-of-types widget '(choose-patient)))

(defun make-patient-editor-widget ()
  (let ((edw (make-instance 'patient-editor)))
    (flet ((delete-patients-and-answers (obj items)
             (declare (ignore obj))
             (delete-patients-and-answers edw items)))
      (let* ((mark-dirty-patient
              (lambda (&rest ignore)
                (declare (ignore ignore))
                (mark-dirty-sibling-widgets-of-types
                 edw '(choose-patient center-editor))))
             (gred (make-instance 'gridedit
                                 :name 'patient-grid
                                 :data-class 'patient
                                 :view 'patient-table-view
                                 :item-form-view 'patient-form-view
                                 :on-query 'patient-editor-query
                                 :on-add-item-completed mark-dirty-patient
                                 :on-edit-item-completed mark-dirty-patient
                                 :no-items-to-delete-format-string
                                 #!"Please select patients to delete"
                                 :delete-confirmation-string-function
                                 'delete-patients-confirmation-string
                                 :on-delete-items #'delete-patients-and-answers)))
        (setf (composite-widgets edw) (list gred))))
    ;; Returns
    edw))

    
