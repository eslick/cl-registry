(in-package :registry)

(defun create-suebyrnes-survey (&key (owner (current-user)))
  (with-transaction ()
    (flet ((choices-options-yes-no-notsure ()
             (radio-options (choices-mirror-alist '("Yes" "No" "Not sure")))))
      (let* ((prompt-format "~*<SUP>~A</SUP>~*&nbsp;~A")
             (survey
              (make-instance 'survey
                             :name "Potential DNA LAM Study"
                             :description "Questions to help with a study being considered to search for potential genetic basis for the development of LAM"
                             :owner owner :published t :priority 1 :diary-p nil
                             :ranking-record (make-ranking-record :ranking nil :distribution nil)))
             (group
              (make-instance 'survey-group
                             :name "Introduction"
                             :advice "<P>Hi Everyone! Thanks to all of you who responded to the email questionnaire that I sent some time ago.
I continue to be amazed by LAM patients and their willingness to support LAM scientists in their work.
Your responses are extremely valuable and, without a doubt, help to move LAM research forward at a faster pace.
We are extremely grateful that you take the time to respond! If I can gather enough interest among LAM families, I would like to help with a study currently being considered to search for the potential genetic basis for the development of LAM.
There would be no cost to you or your family members.
Please take the time to answer the easy questions below by placing your answers underneath the questions. 
<!-- I would appreciate your response by Wednesday, March 21st. -->
Remember that your names will not be shared with anyone.
<P> <B>Thank you so much - I hope to see you at the LAM conference!</B>
<P><A HREF=\"mailto:sbyrnes@thelamfoundation.org\">Sue Byrnes &lt;sbyrnes@thelamfoundation.org&gt;</A>
<BR>Scientific Research Administrator"))
             (q1
              (apply #'make-question "Do you have Tuberous Sclerosis (TS)?"
                     :number 1
                     :prompt-format prompt-format
                     (choices-options-yes-no-notsure)))
             (q2
              (apply #'make-question "Would you be willing to assist with this genetic study by going to a local lab to have blood drawn?"
                     :number 2
                     :prompt-format prompt-format
                     (choices-options-yes-no-notsure)))
             (q3
              (let* ((q3
                      (apply #'make-question "Do you have any biological sisters (not half-sisters)?"
                             :number 3
                             :prompt-format prompt-format
                             (choices-options-yes-no-notsure)))
                     (q3a
                      (make-question "If so, how many sisters do you have who you think would be willing to have blood drawn?"
                                     :number "3a"
                                     :prompt-format prompt-format
                                     :data-type :number))
                     (subgroup (make-survey-sub-group-named group "" :order (list q3a))))
                (add-rule group q3 "Yes" subgroup ':inline)
                q3))
             (q4
              (let* ((q4
                      (apply #'make-question "Is your biological mother living?"
                             :number 4
                             :prompt-format prompt-format
                             (choices-options-yes-no-notsure)))
                     (q4a
                      (apply #'make-question "If so, do you believe she would be willing to go to a local lab to have blood drawn?"
                             :number "4a"
                             :prompt-format prompt-format
                             (choices-options-yes-no-notsure)))
                     (subgroup (make-survey-sub-group-named group "" :order (list q4a))))
                (add-rule group q4 "Yes" subgroup ':inline)
                q4))
             (q5
              (let* ((q5
                      (apply #'make-question "Is your biological father living?"
                             :number 5
                             :prompt-format prompt-format
                             (choices-options-yes-no-notsure)))
                     (q5a
                      (apply #'make-question "If so, do you think he would be willing to go to a local lab to have blood drawn?"
                             :number "5a"
                             :prompt-format prompt-format
                             (choices-options-yes-no-notsure)))
                     (subgroup (make-survey-sub-group-named group "" :order (list q5a))))
                (add-rule group q5 "Yes" subgroup ':inline)
                q5))
             (q6
              (let* ((q6
                      (apply #'make-question "Are you coming to LAMposium 2010?"
                             :number 6
                             :prompt-format prompt-format
                             (choices-options-yes-no-notsure)))
                     (q6a
                      (apply #'make-question "If so, would you be willing to have blood drawn at the conference during a time that is convenient for you?"
                             :number "6a"
                             :prompt-format prompt-format
                             (choices-options-yes-no-notsure)))
                     (subgroup (make-survey-sub-group-named group "" :order (list q6a))))
                (add-rule group q6 "Yes" subgroup ':inline)
                q6))
             (q7
              (let* ((q7
                      (apply #'make-question "Will any of your family members (sisters, mother or father) be attending LAMposium 2010?"
                             :number 7
                             :prompt-format prompt-format
                             (choices-options-yes-no-notsure)))
                     (q7a
                      (make-question "If so, which one(s) would be willing to have blood drawn during the conference?"
                             :number "7a"
                             :prompt-format prompt-format))
                     (subgroup (make-survey-sub-group-named group "" :order (list q7a))))
                (add-rule group q7 "Yes" subgroup ':inline)
                q7)))
        ;; Groups / questions
        (setf (group-questions group) (list q1 q2 q3 q4 q5 q6 q7)
              (survey-groups survey) (list group))
        ;; Returns
        survey))))
