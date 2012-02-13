;; -*- Mode:Lisp; tab-width:2; indent-tabs-mode:nil -*-

;;; Copyright (c) 2008-2010, Massachusetts Institute of;Technology. All rights reserved. 
;;; Copyright (c) 2008-2010, LAM Treatment Alliance. All rights reserved. 
;;; Released under a BSD-style license: http://www.opensource.org/licenses/bsd-license.php 
;;; See LICENSE file 

(in-package :registry)

(defun find-surveys-for-data-dictionary (&key published-only-p (webapp-session-required t))
  (let ((surveys (get-instances-by-class 'survey)))
    (loop for survey in surveys
       append
       (if (or (published-p survey)
               (and (not published-only-p)
                    (or (not webapp-session-required)
                        (is-admin-p)
                        (eq (current-user) (owner survey))
                        (member (current-user) (survey-acl survey)))))
           (list survey)))))

(defwidget data-dictionary ()
  ((published-only-p :accessor published-only-p :initarg :published-only-p :initform nil)
   (duplicates-mode :accessor duplicates-mode :initarg :duplicates-mode :initform nil)))

(defmethod render-widget-body ((widget data-dictionary) &rest args)
  (declare (ignore args))
  (let ((surveys
         (sort (find-surveys-for-data-dictionary :published-only-p (published-only-p widget))
               #'string-lessp :key #'survey-sort-key)))
    (labels ((render-close-link ()
               (with-html
                 (:P (render-link (f* (answer widget)) "Close")))))
      (render-close-link)
      (with-html
        (:DIV
         :CLASS "data-dictionary"
         (:TABLE
          (:TR (:TH :ALIGN "left" "Survey") (:TH :ALIGN "left" "Page") (:TH :ALIGN "left" "Question"))
          (dolist (survey surveys)
            (htm
             (:TR (:TD :COLSPAN 3
                      (:B (str (name survey))) (:BR) (str (description survey))))
             (dolist (group (survey-groups survey))
               (htm
                (:TR (:TD) (:TD (str (group-name group)) (:TD))))
               (dolist (question (group-questions group))
                 (htm
                  (:TR (:TD) (:TD)
                       (:TD (str (question-prompt question)))))))))))
        (render-close-link)))))

(defwidget data-dictionary-button ()
  ((data-dictionary :accessor data-dictionary :initarg :data-dictionary :initform nil)
   (link-title :accessor link-title :initarg :link-title :initform "")))

(defmethod render-widget-body ((widget data-dictionary-button) &rest args)
  (declare (ignore args))
  (with-slots (data-dictionary link-title) widget
    (labels ((dialog-action (&rest args)
               (declare (ignore args))
               (do-dialog "Question List" data-dictionary)))
      (with-html
        (render-link #'dialog-action link-title :class "button")))))

(defun write-data-dictionary (&key published-only-p (stream *standard-output*))
  (let* ((surveys
         (sort (find-surveys-for-data-dictionary :published-only-p published-only-p
                                                 :webapp-session-required nil)
               #'string-lessp :key #'survey-sort-key))
         (output
          (with-html-to-string
            (:DIV
             :CLASS "data-dictionary"
             (:TABLE
              (:TR (:TH :ALIGN "left" "Survey") (:TH :ALIGN "left" "Page") (:TH :ALIGN "left" "Question"))
              (dolist (survey surveys)
                (htm
                 (:TR (:TD :COLSPAN 3
                           (:B (str (name survey))) (:BR) (str (description survey))))
                 (dolist (group (survey-groups survey))
                   (htm
                    (:TR (:TD) (:TD (str (group-name group)) (:TD))))
                   (dolist (question (group-questions group))
                     (htm
                      (:TR (:TD) (:TD)
                           (:TD (str (question-prompt question))))))))))))))
    (princ output stream)))

(defun write-data-dictionary-file (outfile &rest args)
  (with-open-file (out outfile :direction :output :if-exists :new-version)
    (write-line "<HTML><BODY>" out)
    (apply #'write-data-dictionary :stream out args)
    (terpri)
    (write-line "</BODY></HTML>" out))
  t)
