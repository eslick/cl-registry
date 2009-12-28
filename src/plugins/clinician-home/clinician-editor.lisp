;; -*- Mode:Lisp; tab-width:2; indent-tabs-mode:nil -*-

(in-package :registry)

(registry-proclamations)

(defwidget clinician-editor (composite)
  ((visible-p :accessor visible-p :initform nil)))
 
(defmethod render-widget-body ((widget clinician-editor) &rest args)
  (declare (ignore args))
  (let ((clinician (current-clinician)))
    (when (setf (visible-p widget)
                (or (is-admin-p)
                    (and clinician
                         (has-permission-p clinician :center-admin))))
      (with-html
        (:h2 "Add / Edit Clinician")
        (:hr)))))

(defmethod render-widget-children :around ((widget clinician-editor) &rest args)
  (declare (ignore args))
  (when (visible-p widget) (call-next-method)))

(defun clinician-username (clinician)
  (aif (user clinician)
       (username it)
       ""))

(defun clinician-center-short-name (clinician)
  (short-name (center clinician)))

(defun clinician-editor-query (widget sort range &key countp)
  (declare (ignore widget))
  (let* ((res (get-clinicians-for-center (current-center))))
    (cond (countp (length res))
          (t (destructuring-bind (col . dir) (or sort '(nil . nil))
               (unless (eq col 'user)
                 (setf res (sort res (if (eq dir :asc)
                                         #'string-lessp
                                         #'string-greaterp)
                                 :key #'clinician-username)))
               (when sort
                 (destructuring-bind (col . dir) sort
                   (setf res (sort res
                                   (if (eq dir :asc)
                                       #'string-or-nil-lessp
                                       #'string-or-nil-greaterp)
                                   :key (cond ((eq col 'user) #'clinician-username)
                                              ((eq col 'center)
                                               #'clinician-center-short-name)
                                              ((eq col 'permissions)
                                               #'permission-names)))))))
             (when range
               (setf res (subseq res (car range) (cdr range)))))
          res)))

(defun make-clinician-editor-widget ()
  (let ((edw (make-instance 'clinician-editor))
        (gred (make-instance 'gridedit
                             :name 'clinician-grid
                             :data-class 'clinician
                             :view 'clinician-table-view
                             :item-form-view 'clinician-form-view
                             :on-query 'clinician-editor-query)))
    (setf (composite-widgets edw) (list gred))
    ;; Returns
    edw))

    
