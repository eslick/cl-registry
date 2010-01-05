;; -*- Mode:Lisp; tab-width:2; indent-tabs-mode:nil -*-

(in-package :registry)

(registry-proclamations)

(defwidget choose-patient ()
  ((hr-p :accessor choose-patient-hr-p :initarg :hr-p :initform t)))

(defmethod render-widget-body ((widget choose-patient) &rest args)
  (declare (ignore args))
  (let* ((user (current-user))
         (center (current-center))
         (patients (and center (get-patients-for-center center)))
         (user-patient (and user (get-patient-for-user user))))
    (when user-patient
      (pushnew user-patient patients :test #'eq))
    (with-html
      (cond ((null patients)
             (setf (current-patient) nil)
             (htm (str #!"You do not have access to any patients")))
            ((null (cdr patients))
             (let ((patient (car patients)))
               (unless (eq patient (current-patient))
                 (setf (current-patient) patient))
               (htm
                "<b>Patient:</b> "
                (str (id patient)))))
            (t (let ((patient (current-patient))
                     (action (lambda (&key patient &allow-other-keys)
                               (cond ((get-patient patient center t)
                                      (setf (current-patient) patient))
                                     (t
                                      ;; Somebody deleted or renamed the patient
                                      (mark-dirty widget))))))
                 (setf patients (sort patients #'string-lessp :key #'id))
                 (unless (member patient patients :test #'eq)
                   (setf patient (car patients)
                         (current-patient) patient))
                 (with-html-form (:get action :use-ajax-p t :class "autodropdown")
                   "<b>Patient:</b> "
                   (render-dropdown "patient" (mapcar 'id patients)
                                    :selected-value (id patient)
                                    :autosubmitp t)))))
      (when (choose-patient-hr-p widget)
        (htm (:hr))))))

(defun make-choose-patient-widget (&key (hr-p t))
  (make-instance 'choose-patient :hr-p hr-p))
