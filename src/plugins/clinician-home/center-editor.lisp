;; -*- Mode:Lisp; tab-width:2; indent-tabs-mode:nil -*-

(in-package :registry)

(registry-proclamations)

(defwidget center-editor (composite)
  ((visible-p :accessor visible-p :initform nil)))
 
(defmethod render-widget-body ((widget center-editor) &rest args)
  (declare (ignore args))
  (when (setf (visible-p widget) (is-admin-p))
    (with-html
      (:h2 "Add / Delete Center")
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
               (setf res (subseq res (car range) (cdr range)))))
          res)))

(defun make-center-editor-widget ()
  (let ((edw (make-instance 'center-editor))
        (gred (make-instance 'gridedit
                             :name 'center-grid
                             :data-class 'center
                             :view 'center-table-view
                             :item-form-view 'center-form-view
                             :on-query 'center-editor-query)))
    (setf (composite-widgets edw) (list gred))
    ;; Returns
    edw))

    
