;; -*- Mode:Lisp; tab-width:2; indent-tabs-mode:nil -*-

(in-package :registry)

(registry-proclamations)

(defwidget choose-patient ()
  ())

(defmethod render-widget-body ((widget choose-patient) &rest args)
  (declare (ignore args))
  (let* ((user (current-user))
         (center (current-center))
         (patients (and center (get-patients-for-center center)))
         (user-patient (and user (get-patient-for-user user)))
         (invalidate nil))
    (when user-patient
      (pushnew user-patient patients :test #'eq))
    (with-html
      (cond ((null patients)
             (htm (str #!"You do not have access to any patients")))
            ((null (cdr patients))
             (let ((patient (car patients)))
               (unless (eq patient (current-patient))
                 (setf (current-patient) patient
                       invalidate t))
               (htm
                "<b>Patient:</b> "
                (str (id patient)))))
            (t (let ((patient (current-patient))
                     (action (lambda (&key patient &allow-other-keys)
                               (setf (current-patient) patient)
                               (mark-dirty (widget-parent widget)))))
                 (setf patients (sort patients #'string-lessp :key #'id))
                 (unless (member patient patients :test #'eq)
                   (setf patient (car patients)
                         (current-patient) patient
                         invalidate t))
                 (with-html-form (:get action :use-ajax-p t :class "autodropdown")
                   "<b>Patient:</b> "
                   (render-dropdown "patient" (mapcar 'id patients)
                                    :selected-value (id patient)
                                    :autosubmitp t)))))
      (htm (:hr)))
    (when invalidate (mark-dirty (widget-parent widget)))))

(defun make-choose-patient-widget ()
  (make-instance 'choose-patient))
