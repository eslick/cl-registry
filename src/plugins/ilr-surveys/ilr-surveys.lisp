;;; -*- Mode:Lisp; tab-width:2; indent-tabs-mode:nil -*-

;;; Copyright (c) 2008-2010, Massachusetts Institute of;Technology. All rights reserved. 
;;; Copyright (c) 2008-2010, LAM Treatment Alliance. All rights reserved. 
;;; Released under a BSD-style license: http://www.opensource.org/licenses/bsd-license.php 
;;; See LICENSE file 

(in-package :registry)

(define-plugin ilr-surveys ()
  )

;;; Utilities

(defun drop-ilr-surveys (surveys &key force)
  (with-transaction ()
    (cond
      (force
       (mapcar 'drop-instance (get-instances-by-class 'answer))
       (mapcar 'drop-instance (get-instances-by-class 'question))
       (mapcar 'drop-instance (get-instances-by-class 'survey-group))
       (mapcar 'drop-instance (get-instances-by-class 'survey)))
      (t
       (dolist (survey surveys)
         (dolist (group (survey-groups survey))
           (and group (drop-group group)))
         (drop-instance survey))))))

(defmacro dropdown-options (var)
  `(list :data-type :choice :view-type :dropdown :choices ,var))

(defmacro multi-choices-options (var)
  `(list :data-type :multichoice :choices ,var))

(defmacro radio-options (var)
  `(list :data-type :choice :view-type :radio :choices ,var))

(defmacro choices-options-numbered (labels &key (start 1.))
  (let ((countsym (gensym)))
    `(let ((,countsym ,start))
       (loop for item in ,labels
          collect
          (prog1
              (cons
               (format nil "(~D) ~A" ,countsym item)
               ,countsym)
            (incf ,countsym))))))

(defvar *choices-alist-yes-no '(("Yes" . t) ("No" . nil)))

(defmacro choices-options-yes-no () '(radio-options *choices-alist-yes-no))

(defmacro choices-mirror-alist (choices)
  `(loop for str in ,choices
      collect (cons str str)))

(defmacro choices-breaks-alist (choices)
  `(loop for thing in ,choices
        collect
        (multiple-value-bind (car cdr)
            (if (atom thing)
                (values thing thing)
                (values (car thing) (cdr thing)))
          ;; Returns
          (cons (concatenate 'string car "<BR>") cdr))))

(defun formatted-question-number (num &optional stream)
  (and num (format stream "<SUP>~D</SUP>" num)))

;;
;; Survey objects - functional API
;; Hash table contains arrays of questions
;;

(defvar *survey-question-table* (make-hash-table :test 'equal))

(defun make-survey-named (name &rest args)
  (setf (gethash name *survey-question-table*) (make-array 10. :element-type 'question :adjustable t))
  (apply #'make-instance 'survey :name name args))

(defun make-survey-group-named-and-numbered (survey name num &rest args)
  (let* ((groups (survey-groups survey))
         (group
          (apply #'make-instance 'survey-group
                 :owner (owner survey)
                 :name (format nil "~A Section~@[ ~D~]"
                               name
                               (cond
                                 ((null num) nil)
                                 ((numberp num) num)
                                 ((eq num t) (1+ (length groups)))))
                 args)))
    ;;(format t "~&Add group ~S to groups ~S" group groups)
    (if groups
        (setf (survey-groups survey) (append groups (list group)))
        (setf (survey-groups survey) (list group)))
    ;; Returns
    group))


(defun make-survey-sub-group-named (group name &rest args)
  (setq name (or name (gensym)))
  (apply #'make-instance 'survey-group
         :name (format nil "~A ~A" (group-name group) name)
         :owner (owner group) args))

(defgeneric make-question-named-and-numbered (survey name number &rest args))

(defmethod make-question-named-and-numbered ((survey string) name num &rest args)
  (let ((obj (get-survey survey)))
    (aif obj
         (apply #'make-question-named-and-numbered it name num args)
         (error "Survey does not exist: ~S" survey))))

(defmethod make-question-named-and-numbered ((survey survey) (num integer) (name string) &rest args)
  (apply #'make-question-named-and-numbered survey name num args))

(defmethod make-question-named-and-numbered ((survey survey) (name string) (num integer) &rest args)
  (let ((question
         (apply #'make-question name
                :prompt-prefix (formatted-question-number num)
                args))
        (questions (gethash (name survey) *survey-question-table*)))
    ;; Save this question
    (when questions
      (if (>= num (length questions))
          (adjust-array questions (+ num 8.)))
      (setf (aref questions num) question))
    ;; Returns
    question))
