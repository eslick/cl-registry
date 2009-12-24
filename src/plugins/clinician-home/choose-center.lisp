;; -*- Mode:Lisp; tab-width:2; indent-tabs-mode:nil -*-

(in-package :registry)

(registry-proclamations)

(defwidget choose-center ()
  ())

(defmethod render-widget-body ((widget choose-center) &rest args)
  (declare (ignore args))
  (let* ((user (current-user))
         (centers (get-centers-for-user user nil))
         (invalidate nil))
    (with-html
      (cond ((null centers)
             (htm #!"You do not belong to any centers"))
            ((null (cdr centers))
             (let ((center (car centers)))
               (unless (eq center (current-center))
                 (setf (current-center) center
                       invalidate t))
               (htm
                "<b>Center:</b> "
                (str (short-name center))
                " - "
                (str (center-name center)))))
            (t (let ((center (current-center))
                     (action (lambda (&key center &allow-other-keys)
                               (setf (current-center) center)
                               (mark-dirty (widget-parent widget)))))
                 (unless (member center centers :test #'eq)
                   (setf center (car centers)
                         (current-center) center
                         invalidate t))
                 (with-html-form (:get action :use-ajax-p t :class "autodropdown")
                   "<b>Center:</b> "
                   (render-dropdown "center" (mapcar 'short-name centers)
                                    :selected-value (short-name center)
                                    :autosubmitp t)
                   " "
                   (str (center-name center)))))))
    (when invalidate (mark-dirty (widget-parent widget)))))

(defun make-choose-center-widget ()
  (make-instance 'choose-center))
