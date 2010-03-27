;;; -*- Mode:Lisp; tab-width:2; indent-tabs-mode:nil -*-

;;; Copyright (c) 2008-2010, Massachusetts Institute of;Technology. All rights reserved. 
;;; Copyright (c) 2008-2010, LAM Treatment Alliance. All rights reserved. 
;;; Released under a BSD-style license: http://www.opensource.org/licenses/bsd-license.php 
;;; See LICENSE file 

(in-package :registry)

;;;
;;; Define Saint George's survey
;;;

(defun create-sgrq-surveys (&key (owner (current-user)) study-name)
  (with-transaction ()

    (flet ((sgrq-survey-name (partno) 
             (format nil "~@[~A - ~]St George's Respiratory Questionnaire (SGRQ), part ~D" study-name partno))
           (sgrq-survey-description (partno) 
             (format nil "SGRQ: A pulmonary disease related quality of life survey, part ~D" partno)))

      (list

       ;;
       ;; Survey part 1
       ;;
       (let* ((survey1
               (make-instance 'survey
                              :name (sgrq-survey-name 1.)
                              :description (sgrq-survey-description 1.)
                              :owner owner :origin "researcher" :published t
                              :priority 1
                              :diary-p nil
                              :ranking-record (make-ranking-record :ranking nil :distribution nil)))
              (q0
               (apply #'make-question "Please check one box to show how you describe your current health"
                      :prompt-format "Before completing the rest of the questionnaire:<BR>~A:"
                      (radio-options
                       (choices-mirror-alist
                        '("Very good" "Good" "Fair" "Poor" "Very poor")))))
              (p0g0
               (make-instance 'survey-group
                              :name "Cover page"
                              :owner owner
                              :advice "This questionnaire is designed to help us learn much more about how your breathing is troubling you and how it affects your life. We are using it to find out which aspects of your illness cause you most problems, rather than what the doctors and nurses think your problems are.
<P>Please read the instructions carefully and ask if you do not understand anything. Do not spend too long deciding about your answers."
                              :order (list q0))))
         (setf (survey-groups survey1) (list p0g0))
         ;; Returns
         survey1)

       ;;
       ;; Survey part 2
       ;;
       (let* ((survey2
               (make-instance 'survey
                              :name (sgrq-survey-name 2.)
                              :description (sgrq-survey-description 2.)
                              :owner owner :origin "researcher" :published t :priority 2
                              :diary-p nil
                              :ranking-record (make-ranking-record :ranking nil :distribution nil)))
              (p1g1
               (let ((group
                      (make-survey-group-table
                       (:name "SQRQ part 2 Section 1"
                        :advice "Questions about how much chest trouble you have had over the past 4 weeks."
                        :owner owner
                        :default-question-args
                        (:data-type :choice
                                    :view-type :dropdown
                                    :choices '(("almsot every day" . 1)
                                               ("several days a week" . 2)
                                               ("a few days a month" . 3)
                                               ("only with respiratory infections" . 4)
                                               ("not at all" . 5))))
                       (#.(formatted-question-number 1) "Over the past 4 weeks, I have coughed:" (:question))
                       (#.(formatted-question-number 2) "Over the past 4 weeks, I have brought up phlegm (sputum):" (:question))
                       (#.(formatted-question-number 3) "Over the past 4 weeks, I have had shortness of breath:" (:question))
                       (#.(formatted-question-number 4) "Over the past 4 weeks, I have had wheezing attacks:" (:question)))))
                 (setf (survey-groups survey2) (append (survey-groups survey2) (list group)))
                 ;; Returns
                 group))
              (prompt-format-number "~*<SUP>~D</SUP>&nbsp;~*~A")
              (p1q5
               (apply #'make-question "How many times during the past 4 weeks have you suffered from severe or very unpleasant respiratory attacks?"
                      :number 5.
                      :prompt-format prompt-format-number
                      (radio-options
                       (choices-breaks-alist
                        '(("more than 3 attacks" . 4)
                          ("3 attacks" . 3)
                          ("2 attacks" . 2)
                          ("1 attack" . 1)
                          ("no attacks" . 0))))))
              (p1q6
               (apply #'make-question "How long did the worst attack of chest trouble last?"
                      :number 6.
                      :prompt-format (concatenate 'string prompt-format-number
                                                  "<BR><EM>(Go to question 7 if you did not have a severe attack)</EM>")
                      (radio-options
                       (choices-breaks-alist
                        '(("a week or more" . 4)
                          ("3 or more days" . 3)
                          ("1 or 2 days" . 2)
                          ("less than a day" . 1))))))
              (p1q7
               (apply #'make-question "Over the last 4 weeks, in a typical week, how many good days (with few respiratory problems) have you had?"
                      :number 7.
                      :prompt-format prompt-format-number
                      (radio-options
                       (choices-breaks-alist
                        '(("No good days" . 0)
                          ("1 or 2 good days" . 1)
                          ("3 or 4 good days" . 2)
                          ("nearly every day was good" . 3)
                          ("every day was good" . 4))))))
              (p1q8
               (apply #'make-question "If you wheeze, is it worse when you get up in the morning?"
                      :number 8.
                      :prompt-format prompt-format-number
                      (choices-options-yes-no)))
              (p1g2 (make-survey-group-named-and-numbered survey2 "SGRQ part 2" t :order (list p1q5 p1q6 p1q7 p1q8)))
              )
         (declare (ignore p1g1 p1g2))
         ;; Returns
         survey2)

       ;;
       ;; Survey part 3
       ;;
       (let* ((survey3
               (make-instance 'survey
                              :name (sgrq-survey-name 3.)
                              :description (sgrq-survey-description 3.)
                              :owner owner :origin "researcher" :published t :priority 2
                              :diary-p nil
                              :ranking-record (make-ranking-record :ranking nil :distribution nil)))
              ;; Section 1
              (p2g1q1
               (apply #'make-question "How would you describe your respiratory condition?"
                      (radio-options
                       (choices-breaks-alist
                        '(("The most important problem I have" . 1)
                          ("Causes me quite a lot of problems" . 2)
                          ("Causes me a few problems" . 3)
                          ("Causes no problem" . 4))))))
              (p2g1q2
               (apply #'make-question "If you have ever held a job:"
                      (radio-options
                       (choices-breaks-alist
                        '(("My respiratory problems made me stop working altogether" . 1)
                          ("My respiratory problems interfere with my job or made me change my job" . 2)
                          ("My respiratory problems do not affect my job" . 3))))))
              (p2g1
               (make-survey-group-named-and-numbered survey3 "SGRQ part 3" t :order (list p2g1q1 p2g1q2)))
              ;; Section 2
              (p2g2
               (let ((group
                      (make-survey-group-table
                       (:name "SGRQ part 3 Section 2"
                        :advice "These are questions about what activities usually make you feel short of breath <EM>these days</EM>.
<BR>For each statement please choose <EM>one</EM> that applies to you <EM>these days</EM>:"
                        :owner owner
                        :default-question-args
                        (:data-type :choice
                                    :view-type :dropdown
                                    :choices '(("True" . t) ("False" . nil))))
                       ("" "Sitting or lying still" (:question))
                       ("" "Washing or dressing yourself" (:question))
                       ("" "Walking around the home" (:question))
                       ("" "Walking outside on level ground" (:question))
                       ("" "Walking up a flight of stairs" (:question))
                       ("" "Walking up hills" (:question))
                       ("" "Playing sports or other physical activities" (:question)))))
                 ;; Survey groups
                 (setf (survey-groups survey3) (append (survey-groups survey3) (list group)))
                 ;; Returns
                 group))
              ;; Section 3
              (p2g3
               (let ((group
                      (make-survey-group-table
                       (:name "SGRQ part 3 Section 3"
                        :advice "These are more questions about your cough and shortness of breath <EM>these days</EM>.
<BR>For each statement please choose <EM>one</EM> that applies to you <EM>these days</EM>:"
                        :owner owner
                        :default-question-args
                        (:data-type :choice
                                    :view-type :dropdown
                                    :choices '(("True" . t) ("False" . nil))))
                       ("" "Coughing hurts" (:question))
                       ("" "Coughing makes me tired" (:question))
                       ("" "I am short of breath when I talk" (:question))
                       ("" "I am short of breath when I bend over" (:question))
                       ("" "My coughing or breathing disturbs my sleep" (:question))
                       ("" "I get exhausted easily" (:question)))))
                 (setf (survey-groups survey3) (append (survey-groups survey3) (list group)))
                 ;; Returns
                 group))
              ;; Section 4
              (p2g4
               (let ((group
                      (make-survey-group-table
                       (:name "SGRQ part 3 Section 4"
                        :advice "These are questions about other effects that your respiratory problems may have on you <EM>these days</EM>.
<BR>For each statement, please choose <EM>one</EM> that applies to you <EM>these days</EM>:"
                        :owner owner
                        :default-question-args
                        (:data-type :choice
                                    :view-type :dropdown
                                    :choices '(("True" . t) ("False" . nil))))
                       ("" "My cough or breathing is embarrassing in public" (:question))
                       ("" "My respiratory problems are a nuisance to my family, friends or neighbors" (:question))
                       ("" "I get afraid or panic when I cannot catch my breath" (:question))
                       ("" "I feel that I am not in control of my respiratory problems" (:question))
                       ("" "I do not expect my respiratory problems to get any better" (:question))
                       ("" "I have become frail or an invalid because of my respiratory problems" (:question))
                       ("" "Exercise is not safe for me" (:question))
                       ("" "Everything seems too much of an effort" (:question)))))
                 (setf (survey-groups survey3) (append (survey-groups survey3) (list group)))
                 ;; Returns
                 group))
              ;; Section 5
              (p2g5
               (let ((group
                      (make-survey-group-table
                       (:name "SGRQ part 3 Section 5"
                        :advice "These are questions about your respiratory treatment. If you are not receiving treatment go to section 6.
<BR>For each statement, please choose <EM>one</EM> that applies to you <EM>these days</EM>:"
                        :owner owner
                        :default-question-args
                        (:data-type :choice
                                    :view-type :dropdown
                                    :choices '(("True" . t) ("False" . nil))))
                       ("" "My treatment does not help me very much" (:question))
                       ("" "I get embarrassed using my medication in public" (:question))
                       ("" "I have unpleasant side effects from my medication" (:question))
                       ("" "My treatment interferes with my life a lot" (:question)))))
                 (setf (survey-groups survey3) (append (survey-groups survey3) (list group)))
                 ;; Returns
                 group))
              ;; Section 6
              (p2g6
               (let ((group
                      (make-survey-group-table
                       (:name "SGRQ part 3 Section 6"
                        :advice "These are questions about how your activities might be affected by your respiratory problems.
<BR>For each statement, please choose <EM>one</EM> that applies to you <EM>because of your respiratory problems</EM>:"
                        :owner owner
                        :default-question-args
                        (:data-type :choice
                                    :view-type :dropdown
                                    :choices '(("True" . t) ("False" . nil))))
                       ("" "I take a long time to get washed or dressed" (:question))
                       ("" "I cannot take a bath or shower, or I take a long time to do it" (:question))
                       ("" "I walk slower than other people my age, or I stop to rest" (:question))
                       ("" "Jobs such as household chores take a long time, or I have to stop to rest" (:question))
                       ("" "If I walk up one flight of stairs, I have to go slowly or stop" (:question))
                       ("" "If I hurry or walk fast, I have to stop or slow down" (:question))
                       ("" "My breathing makes it difficult to do things such as walk up hills, carry things up stairs, light gardening such as weeding, dance, bowl, or play golf" (:question))
                       ("" "My breathing makes it difficult to do things such as carry heavy loads, dig in the garden or shovel snow, jog or walk briskly (5 miles per hour), play tennis or swim" (:question))
                       ("" "My breathing makes it difficult to do things such as very heavy manual work, ride a bike, run, swim fast, or play competitive sports" (:question)))))
                 (setf (survey-groups survey3) (append (survey-groups survey3) (list group)))
                 ;; Returns
                 group))
              ;; Section 7
              (p2g7
               (let ((group
                      (make-survey-group-table
                       (:name "SGRQ part 3 Section 7"
                        :advice "We would like to know how your respiratory problems usually affect your daily life.
<BR>For each statement please choose <EM>one</EM> that applies to you <EM>because of your respiratory problems</EM>:"
                        :owner owner
                        :default-question-args
                        (:data-type :choice
                                    :view-type :dropdown
                                    :choices '(("True" . t) ("False" . nil))))
                       ("" "I cannot play sports or do other physical activities" (:question))
                       ("" "I cannot go out for entertainment or recreation" (:question))
                       ("" "I cannot go out of the house to do the shopping" (:question))
                       ("" "I cannot do household chores" (:question))
                       ("" "I cannot move far from my bed or chair" (:question)))))
                 (setf (survey-groups survey3) (append (survey-groups survey3) (list group)))
                 ;; Returns
                 group))
              (p2g8
               (let* ((questions
                       (list
                        (make-question "Please write in any other important activities that your chest trouble may stop you from doing"
                                       :prompt "Here is a list of other activities that your respiratory problems may prevent you from doing. (You do not have to check these, they are just to remind you of ways your shortness of breath may affect you):
<ul>
<li>Going for walks or walking the dog
<li>Doing activities or chores at home or in the garden
<li>Sexual intercourse
<li>Going to a place of worship, or a place of entertainment
<li>Going out in bad weather or into smoky rooms
<li>Visiting family or friends or playing with children
</ul>
Please write in any other important activities that your respiratory problems may stop you from doing:"
                                       :data-type :string
                                       :view-type :paragraph)
                        (apply #'make-question "Now, please choose (one only) that you think best describes how your respiratory problems affect you:"
                               (radio-options
                                (choices-breaks-alist
                                 '(("It does not stop me from doing anything I would like to do." . 0)
                                   ("It stops me from doing one or two things I would like to do." . 1)
                                   ("It stops me from doing most of the things I would like to do." . 2)
                                   ("It stops me from doing everything I would like to do.")))))))
                      (group
                       (make-survey-group-named-and-numbered survey3 "SGRQ part 3" t :order questions)))
                 ;; Returns
                 group)))

         ;; Ignore these for now they get attached to the survey as a side effect
         (declare (ignore p2g1 p2g2 p2g3 p2g4 p2g5 p2g6 p2g7 p2g8))

         ;; Returns
         survey3)

       ))))
