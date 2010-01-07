;;; -*- Mode:Lisp; tab-width:2; indent-tabs-mode:nil -*-

;;; Copyright (c) 2008-2010, Massachusetts Institute of;Technology. All rights reserved. 
;;; Copyright (c) 2008-2010, LAM Treatment Alliance. All rights reserved. 
;;; Released under a BSD-style license: http://www.opensource.org/licenses/bsd-license.php 
;;; See LICENSE file 

(in-package :registry)

;;;
;;; Define Saint George's survey
;;;

;;; Globals

(defvar *default-study-owner*)

(defvar *surveys)
(defvar *groups1)
(defvar *groups2)
(defvar *questions-part-0)
(defvar *questions-part-1)
(defvar *questions-part-2.1)
(defvar *questions-part-2.2)
(defvar *questions-part-2.3)
(defvar *questions-part-2.4)
(defvar *questions-part-2.5)
(defvar *questions-part-2.6)
(defvar *questions-part-2.7)

(defvar *choices-alist-days-week
  '("Most days a week"
    "Several days a week"
    "A few days a week"
    "Only with chest infections"
    "Not at all"))

(defmacro choices-options-days-week () '(choices-options-numbered *choices-alist-days-week))

;;; Utilities

(defun drop-sgrq ()
  (drop-ilr-surveys *surveys))

;;; Define questions

(defun create-sgrq (&key (owner (current-user)))
  (let ((*default-study-owner* owner))
    (setq *questions-part-0
	(list
	 (apply #'make-question "Please check one box to show how you describe your current health"
		(choices-options-numbered
		 '("Very good" "Good" "Fair" "Poor" "Very poor")))))
  (setq *questions-part-1
	(list
	 (apply #'make-question "Over the last year, I have coughed"
		(choices-options-days-week))
	 (apply #'make-question "Over the last year, I have brought up phleghm (sputum)"
		(choices-options-days-week))
	 (apply #'make-question "Over the last year, I have had shortness of breath"
		(choices-options-days-week))
	 (apply #'make-question "Over the last year, I have had attacks of wheezing"
		(choices-options-days-week))
	 (apply #'make-question "During the last year, how many severe or very unpleasant attacks of chest trouble have you had"
		(choices-options-numbered
		 '("More than 3 attacks"
		   "3 attacks"
		   "2 attacks"
		   "1 attack"
		   "No attacks (if No, skip to Question 13)")))
	 (apply #'make-question "How long did the worst attack of chest trouble last"
		(choices-options-numbered
		 '("A week or more"
		   "3 or more days"
		   "1 or 2 days"
		   "Less than a day")))
	 (apply #'make-question "Over the last year, in an average week, how many good days (with little chest trouble) have you had"
		(choices-options-numbered
		 '("No good days"
		   "1 or 2 good days"
		   "3 or 4 good days"
		   "Nearly every day is good"
		   "Every day is good")))
	 (apply #'make-question "If you have a wheeze, is it worse in the morning"
		(choices-options-yes-no))))
  (setq *questions-part-2.1
	(list
	 (apply #'make-question "How would you describe your chest condition"
		(choices-options-numbered
		 '("The most important problem I have"
		   "Causes me quite a lot of problems"
		   "Causes me a few problems"
		   "Causes no problem")))
	 (apply #'make-question "If you have ever had paid employment, please check off one of these"
		(choices-options-numbered
		 '("My chest trouble made me stop work"
		   "My chest trouble interferes with my work or made me change my work"
		   "My chest trouble does not affect my work")))))
  (setq *questions-part-2.2
	(list
	 (apply #'make-question "Breathless sitting or lying still"
		:prompt "Sitting or lying still:"
		(choices-options-yes-no))
	 (apply #'make-question "Breathless getting washed or dressed"
		:prompt "Getting washed or dressed"
		(choices-options-yes-no))
	 (apply #'make-question "Breathless walking around the home"
		:prompt "Walking around the home:"
		(choices-options-yes-no))
	 (apply #'make-question "Breathless walking outside on the level"
		:prompt "Walking outside on the level:"
		(choices-options-yes-no))
	 (apply #'make-question "Breathless walking up a flight of stairs"
		:prompt "Walking up a flight of stairs:"
		(choices-options-yes-no))
	 (apply #'make-question "Breathless walking up hills"
		:prompt "Walking up hills:"
		(choices-options-yes-no))
	 (apply #'make-question "Breathless playing sports or games"
		:prompt "Playing sports or games:"
		(choices-options-yes-no))))
  (setq *questions-part-2.3
	(list
	 (apply #'make-question "My cough hurts"
		(choices-options-yes-no))
	 (apply #'make-question "My cough makes me tired"
		(choices-options-yes-no))
	 (apply #'make-question "I am breathless when I talk"
		(choices-options-yes-no))
	 (apply #'make-question "I am breathless when I bend over"
		(choices-options-yes-no))
	 (apply #'make-question "My cough or breathing disturbs my sleep:"
		(choices-options-yes-no))
	 (apply #'make-question "I get exhausted easily"
		(choices-options-yes-no))))
  (setq *questions-part-2.4
	(list
	 (apply #'make-question "My cough or breathing is embarrassing in public"
		(choices-options-yes-no))
	 (apply #'make-question "My chest trouble is a nuisance to my family, friends, or neighbors"
		(choices-options-yes-no))
	 (apply #'make-question "I get afraid or panic when I cannot get my breath"
		(choices-options-yes-no))
	 (apply #'make-question "I feel that I am not in control of my chest problem"
		(choices-options-yes-no))
	 (apply #'make-question "I do not expect my chest to get any better"
		(choices-options-yes-no))
	 (apply #'make-question "I have become frail or an invalid because of my chest"
		(choices-options-yes-no))
	 (apply #'make-question "Exercise is not safe for me"
		(choices-options-yes-no))
	 (apply #'make-question "Everything seems too much of an effort"
		(choices-options-yes-no))))
  (setq *questions-part-2.5
	(list
	 (apply #'make-question "My medication does not help me very much"
		(choices-options-yes-no))
	 (apply #'make-question "I get embarrassed using my medication in public"
		(choices-options-yes-no))
	 (apply #'make-question "I have unpleasant side effects from my medication"
		(choices-options-yes-no))
	 (apply #'make-question "My medication interferes with my life a lot"
		(choices-options-yes-no))))
  (setq *questions-part-2.6
	(list
	 (apply #'make-question "I take a long time to get washed or dressed"
		(choices-options-yes-no))
	 (apply #'make-question "I cannot take a bath or shower, or I take a long time"
		(choices-options-yes-no))
	 (apply #'make-question "I walk slower than other people, or I stop for rests"
		(choices-options-yes-no))
	 (apply #'make-question "Jobs such as housework take a long time, or I have to stop for rests"
		(choices-options-yes-no))
	 (apply #'make-question "If I walk up one flight of stairs, I have to go slowly or stop"
		(choices-options-yes-no))
	 (apply #'make-question "If I hurry or walk fast, I have to stop or slow down"
		(choices-options-yes-no))
	 (apply #'make-question "My breathing makes it difficult to do things such as walk up hills, carry things up stairs, light gardening such as weeding, dancing, or playing golf"
		(choices-options-yes-no))
	 (apply #'make-question "My breathing makes it difficult to do things such as carry heavy loads, dig the garden or shovel snow, jog or walk at 5 miles per hour, play tennis or swim"
		(choices-options-yes-no))
	 (apply #'make-question "My breathing makes it difficult to do things such as very heavy manual work, run, cycle, swim fast or play competitive sports"
		(choices-options-yes-no))))
  (setq *questions-part-2.7
	(list
	 (apply #'make-question "I cannot play sports or games"
		(choices-options-yes-no))
	 (apply #'make-question "I cannot go out for entertainment or recreation"
		(choices-options-yes-no))
	 (apply #'make-question "I cannot go out of the house to do the shopping"
		(choices-options-yes-no))
	 (apply #'make-question "I cannot do housework"
		(choices-options-yes-no))
	 (apply #'make-question "I cannot move far from my bed or chair"
		(choices-options-yes-no))
	 (make-question "Please write in any other important activities that your chest trouble may stop you from doing"
			:prompt "Here is a list of other activities that your chest trouble may prevent you from doing. (You do not have to check them off. They are just to remind you of ways in which your breathlessness may affect you):
<ul>
<li>Going for walks or walking the dog
<li>Doing things at home or in the garden
<li>Sexual intercourse
<li>Going out to church, or place of entertainment
<li>Going out in bad weather or into smoky rooms
<li>Visiting family or friends or playing with children
</ul>
56. Please write in any other important activities that your chest trouble may stop you from doing:"
			:data-type :string
			:view-type :paragraph)
	 (apply #'make-question "Now, would you check off (one only) which you think best describes how your chest affects you"
		(choices-options-numbered
		 '("It does not stop me from doing anything I would like to do."
		   "It stops me from doing one or two things I would like to do."
		   "It stops me from doing most of the things I would like to do."
		   "It stops me from doing everything I would like to do.")))))
  ;; Groups
  (setq *groups1
	(list
	 (make-instance 'survey-group
			:name "SGRQ Cover Page"
			:order *questions-part-0
			:advice "Before completing the rest of the questionnaire:"
			:owner *default-study-owner*)
	 (make-instance 'survey-group
			:name "SQRQ Part 1"
			:order *questions-part-1
			:advice "The following questions ask about how much chest trouble you have had over the last year.  Please check off one answer for each question."
			:owner *default-study-owner*)))
  (setq *groups2
	(list
	 (make-instance 'survey-group
			:name "SQRQ Part 2 Section 1"
			:order *questions-part-2.1
			:owner *default-study-owner*)
	 (make-instance 'survey-group
			:name "SQRQ Part 2 Section 2"
			:order *questions-part-2.2
			:advice "Questions about what activities usually make you feel breathless these days.  For each item, please check off either (1) for True or (2) for False as it applies to you."
			:owner *default-study-owner*)
	 (make-instance 'survey-group
			:name "SQRQ Part 2 Section 3"
			:advice "Here are some more questions about your cough and breathlessness these days. For each item, please check off either (1) for True or (2) for False as it applies to you."
			:order *questions-part-2.3
			:owner *default-study-owner*)
	 (make-instance 'survey-group
			:name "SGRQ Part 2 Section 4"
			:advice "The following questions ask about other effects that your chest trouble may have on you these days. For each item, please check off either (1) for True or (2) for False as it applies to you."
			:order *questions-part-2.4
			:owner *default-study-owner*)
	 (make-instance 'survey-group
			:name "SGRQ Part 2 Section 5"
			:advice "The following are questions about medication. If you are receiving no medication go straight to Section 6. To complete this section, please check off either (1) for True or (2) for False as it applies to you."
			:order *questions-part-2.5
			:owner *default-study-owner*)
	 (make-instance 'survey-group
			:name "SGRQ Part 2 Section 6"
			:advice "These are questions about how your activities might be affected by your breathing. For each question, please check off (1) for True if one or more parts applies to you because of your breathing. Otherwise, check off (2) for False."
			:order *questions-part-2.6
			:owner *default-study-owner*)
	 (make-instance 'survey-group
			:name "SGRQ Part 2 Section 7"
			:advice "We would like to know how your chest trouble usually affects your daily life. Please check off either (1) for True or (2) for False as it applies to you because of your chest trouble. (Remember that True only applies to you if you cannot do something because of your breathing.)"
			:order *questions-part-2.7
			:owner *default-study-owner*)))
  ;; Surveys
  (setq *surveys
	(list
	 (make-instance 'survey
			:name "St George's Respiratory Questionnaire (SGRQ) - Part 1"
			:groups *groups1
			:owner *default-study-owner*
			:published t
			:priority 1
			:diary-p nil
			:ranking-record (make-ranking-record :ranking nil :distribution nil))
	 (make-instance 'survey
			:name "St George's Respiratory Questionnaire (SGRQ) - Part 2"
			:groups *groups2
			:owner *default-study-owner*
			:published t
			:priority 1
			:diary-p nil
			:ranking-record (make-ranking-record :ranking nil :distribution nil))))
  ;; Returns
  *surveys))
