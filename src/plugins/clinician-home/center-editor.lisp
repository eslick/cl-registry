;; -*- Mode:Lisp; tab-width:2; indent-tabs-mode:nil -*-

(in-package :registry)

(registry-proclamations)

(defwidget center-editor (composite)
  ((visible-p :accessor visible-p :initform nil)))
 
(defmethod render-widget-body ((widget center-editor) &rest args)
  (declare (ignore args))
  (when (setf (visible-p widget) (is-admin-p))
    (with-html
      (:h2 "Add / Edit Center")
      (:hr))))

(defmethod render-widget-children :around ((widget center-editor) &rest args)
  (declare (ignore args))
  (when (visible-p widget) (call-next-method)))

(defun center-editor-query (widget sort range &key countp)
  (declare (ignore widget))
  (let* ((res (all-centers)))
    (cond (countp (length res))
          (t (destructuring-bind (col . dir) (or sort '(nil . nil))
               (unless (eq col 'short-name)
                 (setf res (sort res (if (eq dir :asc)
                                         #'string-lessp
                                         #'string-greaterp)
                                 :key #'short-name)))
               (when sort
                 (destructuring-bind (col . dir) sort
                   (setf res (sort res
                                   (if (eq dir :asc)
                                       #'string-or-nil-lessp
                                       #'string-or-nil-greaterp)
                                   :key (cond ((eq col 'short-name) #'short-name)
                                              ((eq col 'name) #'center-name)))))))
             (when range
               (setf res (subseq res (car range) (cdr range))))
             res))))

(defun delete-centers-confirmation-string (obj items)
  (declare (ignore obj))
  (loop
     with ids = (cdr items)
     with center-count = (length ids)
     for id in ids
     for center = (get-object id)
     for patients = (get-instances-by-value 'patient 'center center)
     for clinicians = (get-instances-by-value 'clinician 'center center)
     for answer-cnt = (loop for patient in patients
                         summing (length (get-instances-by-value
                                          'answer 'user patient)))
     summing (length patients) into patient-count
     summing (length clinicians) into clinician-count
     summing answer-cnt into answer-count
     finally
       (return (format nil "Delete ~[~*~;~d center~:;~d centers~]~[~*~;, ~d clinician~:;, ~d clinicians~]~[~*~;, ~d patient~:;, ~d patients~]~[~*~;, ~d survey answer~:;, ~d survey answers~]?"
                       center-count center-count
                       clinician-count clinician-count
                       patient-count patient-count
                       answer-count answer-count))))

(defun delete-centers-etc (widget items)
  (loop
     with current-center = (current-center)
     with current-patient = (current-patient)
     for id in (cdr items)
     for center = (get-object id)
     for patients = (get-instances-by-value 'patient 'center center)
     for clinicians = (get-instances-by-value 'clinician 'center center)
     do
       (dolist (patient patients)
         (with-transaction ()
           (dolist (answer (get-instances-by-value 'answer 'user patient))
             (drop-instance answer))
           (when (eq current-patient patient)
             (setf (current-patient) nil))
           (drop-instance patient)))
       (with-transaction ()
         (dolist (clinician clinicians)
           (drop-instance clinician))
         (when (eq current-center center)
           (setf (current-center) nil))
         (drop-instance center)))
  (mark-dirty-sibling-widgets-of-types widget '(choose-center)))

(defun make-center-editor-widget ()
  (let ((edw (make-instance 'center-editor)))
    (flet ((delete-centers-etc (obj items)
             (declare (ignore obj))
             (delete-centers-etc edw items)))
      (let* ((mark-dirty-center
              (lambda (obj item)
                (declare (ignore obj))
                (mark-dirty-sibling-widgets-of-types
                 edw
                 (if (eq item (current-center))
                     '(choose-center patient-editor clinician-editor)
                     '(choose-center)))))
             (gred (make-instance 'gridedit
                                 :name 'center-grid
                                 :data-class 'center
                                 :view 'center-table-view
                                 :item-form-view 'center-form-view
                                 :on-query 'center-editor-query
                                 :on-add-item-completed mark-dirty-center
                                 :on-edit-item-completed mark-dirty-center
                                 :no-items-to-delete-format-string
                                 #!"Please select centers to delete"
                                 :delete-confirmation-string-function
                                 'delete-centers-confirmation-string
                                 :on-delete-items #'delete-centers-etc)))
        (setf (composite-widgets edw) (list gred))))
    ;; Returns
    edw))

    
