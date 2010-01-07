;;; -*- Mode:Lisp; tab-width:2; indent-tabs-mode:nil -*-

;;; Copyright (c) 2008-2010, Massachusetts Institute of;Technology. All rights reserved. 
;;; Copyright (c) 2008-2010, LAM Treatment Alliance. All rights reserved. 
;;; Released under a BSD-style license: http://www.opensource.org/licenses/bsd-license.php 
;;; See LICENSE file 

(in-package :registry)

(define-plugin ilr-surveys ()
  )

;;; Globals

(defvar *default-study-owner*)

;;; Utilities

(defun drop-ilr-surveys (surveys &key force)
  (cond
    (force
     (mapcar 'drop-instance (get-instances-by-class 'answer))
     (mapcar 'drop-instance (get-instances-by-class 'question))
     (mapcar 'drop-instance (get-instances-by-class 'survey-group))
     (mapcar 'drop-instance (get-instances-by-class 'survey)))
    (t
     (dolist (survey surveys)
       (dolist (group (survey-groups survey))
         (and group (drop-group group)))))))

(defmacro choices-options (var)
  `(list :data-type :choice :view-type :dropdown :choices ,var))

(defmacro radio-options (var)
  `(list :data-type :choice :view-type :radio :choices ,var))

(defmacro choices-options-numbered (labels &key (start 1.))
  (let ((countsym (gensym)))
    `(let ((,countsym ,start))
       (choices-options
  (loop for item in ,labels
        collect
        (prog1
      (cons
       (format nil "(~D) ~A" ,countsym item)
       ,countsym)
    (incf ,countsym)))))))

(defvar *choices-alist-yes-no '(("Yes" . t) ("No" . nil)))

(defmacro choices-options-yes-no () '(radio-options *choices-alist-yes-no))
