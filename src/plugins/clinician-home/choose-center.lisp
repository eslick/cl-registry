;; -*- Mode:Lisp; tab-width:2; indent-tabs-mode:nil -*-

(in-package :registry)

(registry-proclamations)

(defwidget choose-center ()
  ())

(defun apply-to-sibling-widgets-of-types (function widget types)
  (dolist (sibling (widget-children (widget-parent widget)))
    (when (and (not (eq widget sibling))
               (dolist (type types)
                 (when (typep sibling type) (return t))))
      (funcall function sibling))))

(defun mark-dirty-sibling-widgets-of-types (widget types)
  (apply-to-sibling-widgets-of-types #'mark-dirty widget types))

(defmethod render-widget-body ((widget choose-center) &rest args)
  (declare (ignore args))
  (let* ((user (current-user))
         (centers (get-centers-for-user user nil))
         (invalidate nil))
    (flet ((mark-dirty-siblings ()
             (mark-dirty (car (last (widget-parents widget))))))
      (with-html
        (cond ((null centers)
               (setf invalidate t)
               (htm (str #!"You do not have access to any centers")))
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
                                 (cond ((get-center center t)
                                        (setf (current-center) center)
                                        (mark-dirty-siblings))
                                       (t
                                        ;; Somebody deleted or renamed the center
                                        (mark-dirty widget))))))
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
      (when invalidate
        (mark-dirty-siblings)))))

(defun make-choose-center-widget ()
  (make-instance 'choose-center))
