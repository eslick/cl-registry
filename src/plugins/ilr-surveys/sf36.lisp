;;; -*- Mode:Lisp; tab-width:2; indent-tabs-mode:nil -*-

;;; Copyright (c) 2008-2010, Massachusetts Institute of;Technology. All rights reserved. 
;;; Copyright (c) 2008-2010, LAM Treatment Alliance. All rights reserved. 
;;; Released under a BSD-style license: http://www.opensource.org/licenses/bsd-license.php 
;;; See LICENSE file 

(in-package :registry)

;;;
;;; Define Saint George's survey
;;;

(defun create-sf36 (&key (owner (current-user)))
  (let* ((*questions1
          (list
           (apply #'make-question "In general would you say your health is"
                  (radio-options
                   (choices-options-numbered
                    '("Excellent" "Very good" "Good" "Fair" "Poor"))))
           (apply #'make-question "Compared to one year ago, how would you rate your health in general now"
                  :prompt  "<b>Compared to one year ago</b>, how would you rate your health in general <b>now</b>?"
                  (radio-options
                   (choices-options-numbered
                    '("Much better now than one year ago"
                      "Somewhat better now than one year ago"
                      "About the same as one year ago"
                      "Somewhat worse now than one year ago"
                      "Much worse now than one year ago"))))))
         (*group1
          (make-instance 'survey-group
                         :name "SF36 Part 1"
                         :order *questions1
                         :advice "<EM>Instructions:</EM> This survey asks for your views about your health. 
This information will help keep track of how you feel and how well you are able to do your usual activities.
<P>Answer every question by marking the answer as indicated. 
If you are unsure about how to answer a question, give the best answer you can."
                         :owner owner))
         (*questions2
          (loop for name in
               '( "a. Vigorous activities, such as running, lifting heavy objects, participating in strenuous sports"
                 "b. Moderate activities, such as moving a table, pushing a vacuum cleaner, bowling, or playing golf"
                 "c. Lifting or carrying groceries"
                 "d. Climbing several flights of stairs"
                 "e. Climbing one flight of stairs"
                 "f. Bending, kneeling, or stooping"
                 "g. Walking more than a mile"
                 "h. Walking several blocks"
                 "i. Walking one block"
                 "j. Bathing or dressing yourself")
               collect
               (apply #'make-question name :prompt name
                      (radio-options
                       (choices-options-numbered
                        '("Yes, Limited A Lot" "Yes, Limited A Little" "No, Not Limited At All"))))))
         (*group2
          (make-instance 'survey-group
                         :name "SF36 Part 2"
                         :order *questions2
                         :advice "The following items are about activities you might do during a typical day. 
Does <B>your health now limit you</B> in these activities?  If so, how much?"
                         :owner owner))
         (*questions3
          (loop for name in
               '("a. Cut down on the amount of time you spent on work or other activities"
                 "b. Accomplished less than you would like"
                 "c. Were limited in the kind of work or other activities"
                 "d. Had difficulty performing the work or other activities (for example, it took extra effort)")
               collect
               (apply #'make-question name :prompt name (choices-options-yes-no))))
         (*group3
          (make-instance 'survey-group
                         :name "SF36 Part 3"
                         :order *questions3
                         :advice "During the <B>past 4 weeks</B>, have you had any of the following problems with your work or other regular daily activities <B>as a result of your physical health</B>?"
                         :owner owner))
         (*questions4
          (loop for name in
               '("a. Cut down the amount of time you spent on work or other activities"
                 "b. Accomplished less than you would like"
                 "c. Didn’t do work or other activities as carefully as usual")
               collect
               (apply #'make-question name :prompt name (choices-options-yes-no))))
         (*group4
          (make-instance 'survey-group
                         :name "SF36 Part 4"
                         :order *questions4
                         :advice "During the <B>past 4 weeks</B>, have you had any of the following problems with your work or other regular daily activities <B>as a result of any emotional problems</B> (such as feeling depressed or anxious)?"
                         :owner owner))
         (*questions5
          (list
           (apply #'make-question "During the past 4 weeks, to what extent has your physical health or emotional problems interfered with your normal social activities with family, friends, neighbors, or groups"
                  :prompt "During the <B>past 4 weeks</B>, to what extent has your physical health or emotional problems interfered with your normal social activities with family, friends, neighbors, or groups?"
                  (radio-options
                   (choices-options-numbered
                    '("Not at all" "Slightly" "Moderately" "Quite a bit" "Extremely"))))
           (apply #'make-question "How much bodily pain have you had during the past 4 weeks"
                  :prompt "How much bodily pain have you had during the past 4 weeks?"
                  (radio-options
                   (choices-options-numbered
                    '("None" "Very mild" "Mild" "Moderate" "Severe" "Very severe"))))
           (apply #'make-question "During the past 4 weeks, how much did pain interfere with your normal work (including both work outside the home and housework)"
                  :prompt "During the <B>past 4 weeks</B>, how much did pain interfere with your normal work (including both work outside the home and housework)?"
                  (radio-options
                   (choices-options-numbered
                    '("Not at all" "A little bit" "Moderately" "Quite a bit" "Extremely"))))))
         (*group5
          (make-instance 'survey-group
                         :name "SF36 Part 5"
                         :order *questions5
                         :advice nil
                         :owner owner))
         (*group6
          (make-survey-group-table (:name "SF36 Part 6"
                                    :advice "These questions are about how you feel and how things have been with you during the past 4 weeks.
For each question, please give the one answer that comes closest to the way you have been feeling.
How much of the time during the <B>past 4 weeks</B>..."
                                    :owner owner
                                    :default-question-args (:data-type :choice
                                                                       :view-type :dropdown
                                                                       :choices '(("(1) All of the Time" . 1)
                                                                                  ("(2) Most of the Time" . 2)
                                                                                  ("(3) A Good Bit of the Time" . 3)
                                                                                  ("(4) Some of the Time" . 4)
                                                                                  ("(5) A Little of the Time" . 5)
                                                                                  ("(6) None of the Time" . 6))))
                                   ("a." "Did you feel full of pep?" (:question))
                                   ("b." "Have you been a very nervous person?" (:question))
                                   ("c." "Have you felt so down in the dumps that nothing could cheer you up?" (:question))
                                   ("d." "Have you felt calm and peaceful?" (:question))
                                   ("e." "Did you have a lot of energy?" (:question))
                                   ("f." "Have you felt downhearted and blue?" (:question))
                                   ("g." "Did you feel worn out?" (:question))
                                   ("h." "Have you been a happy person?" (:question))
                                   ("i." "Did you feel tired?" (:question))))
         (*questions7
          (list
           (apply #'make-question "During the past 4 weeks, how much of the time has your physical health or emotional problems interfered with your social activities (like visiting with friends, relatives, etc.)"
                  :prompt "During the <B>past 4 weeks</B>, how much of the time has your <B>physical health or emotional problems</B> interfered with your social activities (like visiting with friends, relatives, etc.)?"
                  (radio-options
                   (choices-options-numbered
                    '("All of the time"
                      "Most of the time"
                      "Some of the time"
                      "A little of the time"
                      "None of the time"))))))
         (*group7
          (make-instance 'survey-group
                         :name "SF36 Part 7"
                         :order *questions7
                         :advice nil
                         :owner owner))
         (*questions8
          (loop for name in
               '("a. I seem to get sick a little easier than other people"
                 "b. I am as healthy as anybody I know"
                 "c. I expect my health to get worse"
                 "d. My health is excellent")
               collect
               (apply #'make-question name :prompt name
                      (radio-options
                       (choices-options-numbered
                        '("Definitely True" "Mostly True" "Don't Know" "Mostly False" "Definitely False"))))))
         (*group8
          (make-instance 'survey-group
                         :name "SF36 Part 8"
                         :order *questions8
                         :advice "How TRUE or FALSE is <B>each</B> of the following statements for you?" 
                         :owner owner))
         (*questions9
          (list
           (make-question "Comments" :prompt "Comments:"
                                     :data-type :string :view-type :paragraph)))
         (*group9
          (make-instance 'survey-group
                         :name "SF36 Part 9"
                         :order *questions9
                         :advice nil
                         :owner owner))
         (*survey
          (make-instance 'survey
                         :name "SF-36 Quality of Life Survey"
                         :description "SF-36: a generalized health related quality of life survey"
                         :groups (list *group1 *group2 *group3 *group4 *group5 *group6 *group7 *group8 *group9)
                         :owner owner
                         :origin "researcher"
                         :published t
                         :priority 1
                         :diary-p nil
                         :ranking-record (make-ranking-record :ranking nil :distribution nil))))
    ;; Returns
    *survey))
