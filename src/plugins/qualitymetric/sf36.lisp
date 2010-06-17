;; -*- Mode:Lisp; tab-width:2; indent-tabs-mode:nil; -*-

;;; Copyright (c) 2008-2010, Massachusetts Institute of;Technology. All rights reserved. 
;;; Copyright (c) 2008-2010, LAM Treatment Alliance. All rights reserved. 
;;; Released under a BSD-style license: http://www.opensource.org/licenses/bsd-license.php 
;;; See LICENSE file 

(in-package :registry)

(defun create-qualitymetric-sf36-survey (&key (owner (current-user)))
  (with-transaction ()
    (let* ((survey
            (make-instance 'survey :name "SF-36 Quality of Life Survey Scores"
                                   :description "SF-36: a generalized health related quality of life survey"
                                   :owner owner :origin "researcher" :published t :priority 1 :diary-p nil
                                   :ranking-record (make-ranking-record :ranking nil :distribution nil)))
           (group
            (make-survey-group-named survey "SF-36 Scores" :advice "Enter the score for each scale"))
           questions)
      (flet ((make-q (name &optional (description name))
               (let ((question
                      (make-question name :prompt description :data-type :number)))
                 (push question questions))))
        (make-q "GH" "General Health Score")
        (make-q "PF" "Physical Functioning Score")
        (make-q "RP" "Role Physical Score")
        (make-q "BP" "Bodily Pain Score")
        (make-q "VT" "Vitality Score")
        (make-q "SF" "Social Functioning Score")
        (make-q "MH" "Mental Health Score")
        (make-q "RE" "Role Emotional Score")
        (make-q "MCS" "Mental Component Score")
        (make-q "PCS" "Physical Component Score")
        (make-q "RR" "Risk Ratio Score") ;Note: we have not seen "RR" in survey results
        )
      (setf (group-questions group) (reverse questions))
      ;; Returns
      survey)))
