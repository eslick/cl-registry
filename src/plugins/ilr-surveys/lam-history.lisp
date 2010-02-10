;;; -*- Mode:Lisp; tab-width:2; indent-tabs-mode:nil -*-

;;; Copyright (c) 2008-2010, Massachusetts Institute of;Technology. All rights reserved. 
;;; Copyright (c) 2008-2010, LAM Treatment Alliance. All rights reserved. 
;;; Released under a BSD-style license: http://www.opensource.org/licenses/bsd-license.php 
;;; See LICENSE file 

(in-package :registry)

;;;
;;; Define LAM History survey
;;;

;;; Globals

(defconstant +survey-name-lam-history+ "LAM History")

;; Create LAM history survey

(defun create-lam-history (&key (owner (current-user)))
  (with-transaction ()
    (let* ((prompt-format-colon "~A:")
           (prompt-format-question "~A?")
           (prompt-format-numbered-colon "~*<SUP>~D</SUP>~*&nbsp;~A:")
           (prompt-format-numbered-question "~*<SUP>~D</SUP>~*&nbsp;~A?")
           (survey
            (make-instance 'survey :name +survey-name-lam-history+
                                   :description "This study involves a retrospective medical record review, with a particular focus on pulmonary function tests."
                                   :owner owner
                                   :published t
                                   :priority 1
                                   :diary-p nil
                                   :ranking-record (make-ranking-record :ranking nil :distribution nil)))
           (survey-treatment
            (make-instance 'survey
                           :name "LAM History Pneumothorax or Pulmonary Effusion Treatment Diary"
                           :description "Enter information about surgery/treatments for LAM patient."
                           :owner owner
                           :published t
                           :priority 1
                           :diary-p t
                           :diary-description "One result per date"
                           :ranking-record (make-ranking-record :ranking nil :distribution nil)))
           (survey-pft
            (make-instance 'survey
                           :name "LAM History Pulmonary Function Test (PFT) Diary"
                           :description "Enter PFT results for LAM patient."
                           :owner owner
                           :published t
                           :priority 1
                           :diary-p t
                           :diary-description "One result per date"
                           :ranking-record (make-ranking-record :ranking nil :distribution nil)))
           (survey-6mwd
            (make-instance 'survey
                           :name "LAM History Six Minute Walking Distance (6MWD) Diary"
                           :description "Enter 6MWD results for LAM patient."
                           :owner owner
                           :published t
                           :priority 1
                           :diary-p t
                           :diary-description "One result per date"
                           :ranking-record (make-ranking-record :ranking nil :distribution nil)))
           (survey-sgrq
            (make-instance 'survey
                           :name "LAM History Saint George's Respiratory Questionnaire (SGRQ) Diary"
                           :description "Enter SGRQ results for LAM patient."
                           :owner owner
                           :published t
                           :priority 1
                           :diary-p t
                           :diary-description "One result per date"
                           :ranking-record (make-ranking-record :ranking nil :distribution nil))))

      ;;
      ;; Main survey - LAM History
      ;;

      ;;
      ;; Group 1 - clinician header page
      ;;
      (make-survey-group-named-and-numbered survey +survey-name-lam-history+ t
                                            :advice "<B>ILR Clinician Header</B>
<P>Thank you for your involvement in this research study and your collaboration with the International LAM Registry.
<P>We will use this information to examine whether women diagnosed with LAM under age 25 have a more rapid pulmonary function decline relative to those over the age of 55.
<P>This study involves a retrospective medical record review, with a particular focus on pulmonary function tests.  
In completing the following form, we ask that you comply with your local laws and regulations regarding access and use of patient records, and obtain consent whenever indicated.
Data identifiers such as name and date of birth will not be used, and each record will be given a unique identifier for the purpose of data storage.
For further information about data use please see our
<A HREF=\"/forms/ilr-data-use\">International LAM Registry data use form</A>.
<P>For this study, please complete the following data entry form for <B>current</B> and <B>former</B> LAM patients in your care who were:
<UL>
<LI>diagnosed with LAM at age 25 or under
<LI>diagnosed with LAM at age 55 or older
</UL>
<P>The time it takes to complete the following data entry form will vary, depending on the type of medical records you are using and the number of pulmonary function tests to enter.  
You may save your work at any point to complete at a later time.
<P> Additionally, please encourage all of your LAM patients to register on LAMsight.org!")

      ;;
      ;; Group 2 - Patient info
      ;;
      (let* ((q1
              (make-question "Country where patient receives medical care" :number 1.
                             :prompt-format prompt-format-numbered-colon
                             :data-type :string))
             (q2
              (make-question "Date of diagnosis of LAM or TSC-LAM" :number 2.
                             :prompt-format prompt-format-numbered-colon
                             :data-type :date))
             (q3
              (make-question "Age at time of diagnosis" :number 3.
                             :prompt-format prompt-format-numbered-colon
                             :help "(years)"
                             :data-type :number))
             (q4
              (apply #'make-question "Diagnosis type" :number 4.
                     :prompt-format prompt-format-numbered-colon
                     (radio-options
                      '(("Sporadic LAM" . "LAM")
                        ("Tuberous Sclerosis Complex and LAM (TSC-LAM)" . "TSC-LAM")
                        ("Unknown" . "Unknown")))))
             ;; Rule: if q4 answer is TSC-LAM...
             (q5
              (apply #'make-question "If the patient has TSC-LAM, what symptoms does the patient have" :number 5.
                     :prompt-format prompt-format-numbered-question
                     (multi-choices-options
                      (choices-mirror-alist
                       '("Developmental delay" "Behavioral problem" "Seizures" "Other" "None" "Unknown")))))
             ;; Rule: if q5 answer included Other...
             (q5o
              (make-question "Other TSC-LAM symptoms" :data-type :string :view-type :text-field))
             ;; Rule: if q4 answer is TSC-LAM...
             (q6
              (apply #'make-question "If the patient has TSC-LAM, what organ systems are affected by tumors" :number 6.
                     :prompt-format prompt-format-numbered-question
                     (multi-choices-options
                      (choices-mirror-alist
                       '("Brain" "Kidneys" "Heart" "Eyes" "Skin" "Lungs" "Other" "Unknown/Not screened")))))
             ;; Rule: if previous answer included Other...
             (q6o
              (make-question "Other TSC-LAM symptoms" :data-type :string :view-type :text-field))
             (*group* (make-survey-group-named-and-numbered survey +survey-name-lam-history+ t :order (list q1 q2 q3 q4)))
             ;; Subgroups
             (subgroup1 (make-survey-sub-group-named *group* nil :order (list q5 q6)))
             (subgroup2 (make-survey-sub-group-named *group* nil :order (list q5o)))
             (subgroup3 (make-survey-sub-group-named *group* nil :order (list q6o))))
        ;; Group rules
        (add-rule *group* q4 "TSC-LAM" subgroup1 ':successor)
        (add-rule subgroup1 q5 "Other" subgroup2 ':successor)
        (add-rule subgroup1 q6 "Other" subgroup3 ':successor))

      ;;
      ;; Group 3 - How was LAM diagnosed?
      ;;
      (let* ((*group* (make-survey-group-named-and-numbered survey +survey-name-lam-history+ t))
             (q7
              (let* ((question-diagnosis
                      (apply #'make-question "How was LAM diagnosed" :number 7.
                             :prompt-format prompt-format-numbered-question
                             (radio-options
                              (choices-breaks-alist
                               '("Lung biopsy" ; see group rule below
                                 "CT thorax and other tissue biopsy" ;see group rule below
                                 "CT thorax and AML" "CT thorax and chylous collection"
                                 "CT thorax and TSC" "CT thorax and VEGF-D >800"
                                 "Other" ;see group rule below
                                 "Unknown")))))
                     (subgroup-lung-biopsy
                      (make-survey-sub-group-named *group* "diagnosis lung biopsy subgroup"))
                     (question-biopsy-type
                      (let ((question
                             (apply #'make-question "If lung biopsy, please specify type"
                                    (radio-options
                                     (choices-breaks-alist
                                      '(("Open lung biopsy" . "open")
                                        ("Video-assisted thorascopic surgery (VATS) biopsy" . "VATS")
                                        ("Transbronchial lung biopsy" . "Transbronchial")
                                        "Other" ;see group rule below
                                        "Unknown")))))
                            (subgroup-other
                             (make-survey-sub-group-named subgroup-lung-biopsy "diagnosis lung biopsy other subgroup"))
                            (question-other
                             (make-question "Other lung biopsy type - Please specify")))
                        ;; Group
                        (setf (group-questions subgroup-other) (list question-other))
                        (add-rule subgroup-lung-biopsy question "Other" subgroup-other ':inline)
                        ;; Returns
                        question))
                     (subgroup-ct-thorax-other-tissue-biopsy
                      (make-survey-sub-group-named *group* "diagnosis ct thorax other tissue biopsy subgroup"))
                     (question-ct-other-biopsy-type
                      (make-question "CT thorax and other tissue biopsy - Please specity"))
                     (subgroup-other
                      (make-survey-sub-group-named *group* "diagnosis other subgroup"))
                     (question-other
                      (make-question "Other - Please specity")))
                ;; Group
                (setf (group-questions subgroup-lung-biopsy) (list question-biopsy-type))
                (add-rule *group* question-diagnosis "Lung biopsy" subgroup-lung-biopsy ':inline)
                (setf (group-questions subgroup-ct-thorax-other-tissue-biopsy) (list question-ct-other-biopsy-type))
                (add-rule *group* question-diagnosis "CT thorax and other tissue biopsy" subgroup-ct-thorax-other-tissue-biopsy ':inline)
                (setf (group-questions subgroup-other) (list question-other))
                (add-rule *group* question-diagnosis "Other" subgroup-other ':inline)
                ;; Returns
                question-diagnosis)))
        ;; Group
        (setf (group-questions *group*) (list q7)))

      ;;
      ;; Group 4
      ;;
      (let* ((*group*
              (make-survey-group-named-and-numbered survey +survey-name-lam-history+ t
               :advice "<SUP>8</SUP> If a biopsy was performed, what were the histopathological findings?"))
             (*questions*
              (loop for spec in
                   '(("HMB-45 immunostaining" (("Completed" "Positive" "Negative") ("Not completed")))
                     ("Estrogen receptors" (("Completed" "Positive" "Negative" "Immunostaining" "Tissue quantitative analysis") ("Not completed")))
                     ("Progesterone receptors" (("Completed" "Positive" "Negative" "Immunostaining" "Tissue quantitative analysis") ("Not completed")))
                     ("Gene mutation analysis" (("Completed" "TSC 1 mutation" "TSC 2 mutation" "Other") ("Not completed"))))
                   as name = (first spec)
                   as options = (second spec)
                   collect
                 ;; !! TODO: Generalize this as recursive function !!
                   (let ((question
                          (apply #'make-question name
                                 (radio-options (choices-breaks-alist (mapcar #'first options))))))
                     (loop for option in options
                        as value = (first option)
                        as choices = (rest option)
                        when choices
                        do
                        (let* ((question8a
                                (apply #'make-question (format nil "~A ~A" name (gensym))
                                       :prompt nil
                                       (radio-options (choices-breaks-alist choices))))
                               (subgroup1 (make-survey-sub-group-named *group* nil :order (list question8a)))
                               (question8o (make-question "Other"))
                               (subgroup2 (make-survey-sub-group-named *group* nil :order (list question8o))))
                          (add-rule *group* question value subgroup1 ':inline)
                          (add-rule subgroup1 question8a "Other" subgroup2 ':inline)))
                     ;; Return 
                     question))))
        (setf (group-questions *group*) *questions*))

      ;;
      ;; Group 5
      ;;
      (let* ((q9
              (apply #'make-question "How did the patient originally present" :number 9.
                     :prompt-format (concatenate 'string prompt-format-numbered-question
                                                 " What symptom, finding or event led to the eventual diagnosis of LAM?")
                     (radio-options
                      (choices-breaks-alist
                       '("Exertional dyspnea"
                         "Spontaneous pneumothorax"
                         "Pleural effusion"
                         "Chylous ascites"
                         "Asthma-like symptoms"
                         "Chest x-ray or CT thorax for another reason"
                         "Other"
                         "Unknown")))))
             ;; Rule: if q9 included Other...
             (q9o
              (make-question "Other symptom(s) originally presented" :prompt "Other:" :data-type :string))
             (q10
              (make-question "How old was the patient at time of the first symptoms attributed to LAM" :number 10.
                             :prompt-format prompt-format-numbered-question
                             :help "(years)" :data-type :number))
             (q11
              (apply #'make-question "What is the patient's smoking history" :number 11.
                     :prompt-format prompt-format-numbered-question
                     (radio-options
                      (choices-breaks-alist
                       '("Current smoker" "Former smoker" "Never a smoker" "Unknown")))))
             (q11a
              (make-question "On average the patient smokes how many packs per day"
                             :prompt-format prompt-format-question
                             :data-type :number))
             (q11b
              (make-question "How many years has the patient smoked"
                             :prompt-format prompt-format-question
                             :data-type :number))
             (q11c
              (make-question "Total pack years"
                             :prompt "<B>Or</B> enter total pack years:"
                             :data-type :number))
             (q12
              (apply #'make-question "Has the patient ever had a pneumothorax" :number 12.
                     :prompt-format prompt-format-numbered-question
                     (radio-options
                      (choices-mirror-alist
                       '("Yes" "No" "Unknown")))))
             (q12a
              (make-question "If yes how many total"
                             :prompt-format prompt-format-question :data-type :number))
             (q12b
              (make-question "If yes how many in the past year"
                             :prompt-format prompt-format-question :data-type :number))
             (q13
              (apply #'make-question "Has the patient ever had a pleural effusion" :number 13.
                     :prompt-format prompt-format-numbered-question
                     (radio-options
                      (choices-mirror-alist
                       '("Yes" "No" "Unknown")))))
             (*group*
              (make-survey-group-named-and-numbered survey +survey-name-lam-history+ t :order (list q9 q10 q11 q12 q13)))
             ;; Subgroups
             (subgroup9o (make-survey-sub-group-named *group* nil :order (list q9o)))
             (subgroup11 (make-survey-sub-group-named *group* nil :order (list q11a q11b q11c)))
             (subgroup12 (make-survey-sub-group-named *group* nil :order (list q12a q12b))))
        ;; Group rules
        (add-rule *group* q9 "Other" subgroup9o ':inline)
        (add-rule *group* q11 "Current smoker" subgroup11 ':inline)
        (add-rule *group* q12 "Yes" subgroup12 ':inline))

      ;;
      ;; Group 6
      ;;
      (let* ((q14
              (apply #'make-question "What extrapulmonary lesions does the patient have" :number 14.
                     :prompt-format prompt-format-numbered-question
                     (multi-choices-options
                      (choices-mirror-alist
                       '("Renal angiomyolipoma" "Non-renal angiomyolipoma" "Lymphangioleiomyoma"
                         "Chylous ascites" "Chylous pleural effusion"
                         "Other" "None")))))
             (*group* (make-survey-group-named-and-numbered survey +survey-name-lam-history+ t :order (list q14)))
             (q14a
              (apply #'make-question "Renal angiomyolipoma - Please indicate location"
                     (radio-options
                      (choices-mirror-alist '("Left" "Right" "Both")))))
             (q14b
              (apply #'make-question "Non-renal angiomyolipoma - Please indicate location"
                     (radio-options
                      (choices-mirror-alist '("Lung" "Liver" "Pancreas" "Other")))))
             (q14b2 (make-question "Other location"))
             (q14c
              (apply #'make-question "Chylous pleural effusion - Please indicate location"
                     (radio-options
                      (choices-mirror-alist '("Left" "Right" "Both")))))
             (q14o (make-question "Other extrapulmonary lesions" :data-type :string))
             ;; Subgroups
             (subgroup14a (make-survey-sub-group-named *group* nil :order (list q14a)))
             (subgroup14b (make-survey-sub-group-named *group* nil :order (list q14b)))
             (subgroup14b2 (make-survey-sub-group-named *group* nil :order (list q14b2)))
             (subgroup14c (make-survey-sub-group-named *group* nil :order (list q14c)))
             (subgroup14o (make-survey-sub-group-named *group* nil :order (list q14o))))
        ;; Group rules
        (add-rule *group* q14 "Renal angiomyolipoma" subgroup14a ':inline)
        (add-rule *group* q14 "Non-renal angiomyolipoma" subgroup14b ':inline)
        (add-rule subgroup14b q14b "Other" subgroup14b2 ':inline)
        (add-rule *group* q14 "Chylous pleural effusion" subgroup14c ':inline)
        (add-rule *group* q14 "Other" subgroup14o ':inline))

      ;;
      ;; Group 7
      ;;
      (let* ((q15
              (make-question "What was the patient's age of menarche" :number 15.
                             :prompt-format prompt-format-numbered-question
                             :help "(years)"
                             ;; ?? TODO "Unknown" checkbox for Q15 ??
                             :data-type :number))
             (q16
              (apply #'make-question "Did the patient take oral contraceptive pills before diagnosed with LAM" :number 16.
                     :prompt-format prompt-format-numbered-question
                     (radio-options
                      (choices-mirror-alist
                       '("Yes" "No" "Unknown")))))
             (q16a
              (make-question "Please specify type of contraceptive" :data-type :string))
             (q16b
              (make-question "Dates of use" :data-type :date-range))
             (q17
              (apply #'make-question "Has the patient ever been pregnant" :number 17.
                     :prompt-format prompt-format-numbered-question
                     (radio-options
                      (choices-mirror-alist
                       '("Yes" "No" "Unknown")))))
             (q17-table
              (make-survey-group-table (:name "question 17 if yes" :advice "If yes: please complete:" :default-question-args (:data-type :number))
                                       ( nil "Number <B>before<B> diagnosis of LAM"
                                             "Number <B>during</B> or <B>after</b> diagnosis of LAM" )
                                       ( "Full term or premature births" (:question) (:question) )
                                       ( "Miscarriages"  (:question) (:question) )
                                       ( "Abortions" (:question) (:question) )))

             (q18
              (apply #'make-question "Has the patient gone through menopause" :number 18.
                     :prompt-format prompt-format-numbered-question
                     (radio-options
                      (choices-mirror-alist
                       '("Yes" "No" "Unknown")))))
             (q18a
              (make-question "Age at time of menopause"
                             :prompt "If yes, age at time of menopause?"
                             :help "(years)" :data-type :number))
             (*group*
              (make-survey-group-named-and-numbered survey +survey-name-lam-history+ t :order (list q15 q16 q17 q18)))
             ;; Subgroups
             (subgroup16
              (make-survey-sub-group-named *group* "Dates of contraceptive use." :order (list q16a q16b)))
             (subgroup18 (make-survey-sub-group-named *group* nil :order (list q18a))))
        ;; Group rules
        (add-rule *group* q16 "Yes" subgroup16 ':inline)
        (add-rule *group* q17 "Yes" q17-table ':inline)
        (add-rule *group* q18 "Yes" subgroup18 ':inline))

      ;;
      ;; Group 8
      ;;
      (let* ((*group*
              (make-survey-group-named-and-numbered survey +survey-name-lam-history+ t
                                                    :advice "<SUP>19</SUP> What is the patient's LAM related <B>treatment</B> history? <SMALL>Please check all that apply</SMALL>"))
             (hormone-therapy/question
              (apply #'make-question "Hormone therapy" (choices-options-yes-no)))
             (hormone-therapy/table
              (make-survey-group-table
               (:name "hormone therapy table" :default-question-args (:data-type :date))
               (nil "Treatment" nil "Start Date" "End Date")
               ((:question :name "hormone therapy: Gn-RH agonist" :data-type :boolean :view-type :checkbox )
                "Gn-RH agonist" nil
                (:question :name "hormone therapy: Gn-RH agonist: from") (:question :name "hormone therapy: Gn-RH agonist: to"))
               ((:question :name "hormone therapy: Progesterone" :data-type :boolean :view-type :checkbox )
                "Progesterone" nil
                (:question :name "hormone therapy: Progesterone: from") (:question :name "hormone therapy: Progesterone: to"))
               ((:question :name "hormone therapy: Tamoxifen" :data-type :boolean :view-type :checkbox )
                "Tamoxifen" nil
                (:question :name "hormone therapy: Tamoxifen: from") (:question :name "hormone therapy: Tamoxifen: to"))
               ((:question :name "hormone therapy: surgical oophorectomy" :data-type :boolean :view-type :checkbox )
                "Surgical oophorectomy" nil
                (:question :name "hormone therapy: surgical oophorectomy: date") nil #+NIL (:question :name "hormone therapy: surgical oophorectomy: to"))
               ((:question :name "hormone therapy: other" :data-type :boolean :view-type :checkbox )
                "Other" (:question :data-type :string)
                (:question :name "hormone therapy: other: from") (:question :name "hormone therapy: other: to"))))
             (bronchiactasis/question
              (apply #'make-question "Bronchodilator/Pulminary medications" (choices-options-yes-no)))
             (bronchiactasis/table
              (make-survey-group-table
               (:name "bronchodilator/pulminary medications table" :default-question-args (:data-type :date))
               (nil "Treatment" nil "Start Date" "End Date")
               ((:question :name "bronch/pulm meds therapy: long acting B agonist" :data-type :boolean :view-type :checkbox )
                "Long acting B agonist" nil (:question) (:question))
               ((:question :name "bronch/pulm meds therapy: oral B agonist" :data-type :boolean :view-type :checkbox )
                "Oral B agonist" nil
                (:question :name "bronch/pulm meds therapy: oral B agonist: from") (:question :name "bronch/pulm meds therapy: oral B agonist: to"))
               ((:question :name "bronch/pulm meds therapy: transdermal B agonist" :data-type :boolean :view-type :checkbox )
                "Transdermal B agonist" nil
                (:question :name "bronch/pulm meds therapy: transdermal B agonist: from") (:question :name "bronch/pulm meds therapy: transdermal B agonist: to"))
               ((:question :name "bronch/pulm meds therapy: anti-cholinergic" :data-type :boolean :view-type :checkbox )
                "Anti-cholinergic" nil
                (:question :name "bronch/pulm meds therapy: anti-cholinergic: from") (:question :name "bronch/pulm meds therapy: anti-cholinergic: to"))
               ((:question :name "bronch/pulm meds therapy: Aminophylline" :data-type :boolean :view-type :checkbox )
                "Aminophylline" nil
                (:question :name "bronch/pulm meds therapy: Aminophylline: from") (:question :name "bronch/pulm meds therapy: Aminophylline: to"))
               ((:question :name "bronch/pulm meds therapy: inhaled steroids" :data-type :boolean :view-type :checkbox )
                "Inhaled steroids" nil
                (:question :name "bronch/pulm meds therapy: inhaled steroids: from") (:question :name "bronch/pulm meds therapy: inhaled steroids: to"))
               ((:question :name "bronch/pulm meds therapy: other" :data-type :boolean :view-type :checkbox )
                "Other" (:question :data-type :string)
                (:question :name "bronch/pulm meds therapy: other: from") (:question :name "bronch/pulm meds therapy: other: to"))))
             (other/meds/question
              (apply #'make-question "Other medical treatment" (choices-options-yes-no)))
             (other/med/table
              (make-survey-group-table
               (:name "other medical treatment table" :default-question-args (:data-type :date))
               (nil "Treatment" nil "Start Date" "End Date")
               ((:question :name "other medical treatment: Sirolimus/Rapamune" :data-type :boolean :view-type :checkbox )
                "Sirolimus/Rapamune" nil
                (:question :name "other medical treatment: Sirolimus/Rapamune: from") (:question :name "other medical treatment: Sirolimus/Rapamune: to"))
               ((:question :name "other medical treatment: other" :data-type :boolean :view-type :checkbox )
                "Other" (:question :name "other medical treatment: other: specify" :data-type :string)
                (:question :name "other medical treatment: other: from") (:question :name "other medical treatment: other: to"))))
             (pneumothorax/pleural-effusion/question
              (let* ((question
                      (apply #'make-question "Pneumothorax or pleural effusion treatment" (choices-options-yes-no)))
                     (question2
                      (apply #'make-question "Confirm that all treatments for pneumothorax or pleural effusion have been entered for the patient"
                             :prompt-format "Please enter <B>all</B> information on treatments for pneumothorax or pleural effusion on the separate Pneumothorax Or Pleural Effusion Treatment Diary.<BR>~A."
                             (choices-options-yes-no)))
                     (subgroup
                      (make-survey-sub-group-named *group* "pneumothorax/pleural-effusion/question subgroup" :order (list question2))))
                ;; Group rules
                (add-rule *group* question t subgroup ':inline)
                ;; Returns
                question))
             (other/surgery/question
              (apply #'make-question "Other surgery" (choices-options-yes-no)))
             (other/surgery/table
              (make-survey-group-table
               (:name "other surgery table" :default-question-args (:data-type :date))
               (nil "Surgery" nil "Date")
               ((:question :name "other surgery: thoracic duct ligation" :data-type :boolean :view-type :checkbox )
                "Thoracic duct ligation" nil (:question :name "other surgery: thoracic duct ligation: date"))
               ((:question :name "other surgery: nephrectomy" :data-type :boolean :view-type :checkbox )
                "Nephrectomy" nil (:question :name "other surgery: nephrectomy: date"))
               ((:question :name "other surgery: hysterectomy" :data-type :boolean :view-type :checkbox )
                "Hysterectomy" nil (:question :name "other surgery: hysterectomy: date"))
               ((:question :name "other surgery: other" :data-type :boolean :view-type :checkbox )
                "Other" (:question :name "other surgery: other: specify" :data-type :string)
                (:question :name "other surgery: other: date"))))
             (transplant/question
              (let* ((question
                      (apply #'make-question "Transplant/Transplant Evaluation"
                             (radio-options
                              (choices-breaks-alist
                               '(("The patient has not required transplant evaluation" . "none")
                                 ("The patient was evaluated, but has not had a transplant" . "evaluated")
                                 ("The patient had a transplant" . "transplant"))))))
                     (question2
                      (apply #'make-question "transplant lungs" :prompt ""
                             (radio-options '(("One lung" . 1) ("Both lungs" . 2)))))
                     (question3 (make-question "Transplant date" :data-type :date))
                     (subgroup
                      (make-survey-sub-group-named *group* "transplant/transplant evaluation question subgroup"
                                                   :order (list question2 question3))))
                ;; Group rules
                (add-rule *group* question "transplant" subgroup ':inline)
                ;; Returns
                question)))
        ;; Group rules
        (add-rule *group* hormone-therapy/question t hormone-therapy/table ':inline)
        (add-rule *group* bronchiactasis/question t bronchiactasis/table ':inline)
        (add-rule *group* other/meds/question t other/med/table ':inline)
        (add-rule *group* other/surgery/question t other/surgery/table ':inline)
        ;; Questions
        (setf (group-questions *group*)
              (list hormone-therapy/question bronchiactasis/question other/meds/question
                    pneumothorax/pleural-effusion/question other/surgery/question
                    transplant/question)))

      ;;
      ;; Group 9
      ;;
      (let* ((q20
              (apply #'make-question "Does/did the patient use oxygen at home" :number 20.
                     :prompt-format prompt-format-numbered-question
                     (choices-options-yes-no)))
             (*group* (make-survey-group-named-and-numbered survey +survey-name-lam-history+ t :order (list q20)))
             (q20a
              (apply #'make-question "If yes the patient uses oxygen at home"
                     :prompt "If yes:"
                     (multi-choices-options
                      (choices-mirror-alist
                       '("Continuous" "With activities" "At night" "Other")))))
             (q20b (make-question "Average #liters/min"
                                  :prompt-format prompt-format-colon
                                  :data-type :number))
             (q20c
              (make-question "When did the patient initiate home oxygen"
                             :prompt-format prompt-format-question
                             :data-type :date))
             (subgroup20
              (make-survey-sub-group-named *group* nil :order (list q20a q20b q20c))))
        ;; Group rules
        (add-rule *group* q20 t subgroup20 ':successor))

      ;;
      ;; Group 10
      ;;
      (let* ((q21
              (apply #'make-question "What is the patient's vital status" :number 21.
                     :prompt-format prompt-format-numbered-question
                     (radio-options
                      (choices-mirror-alist
                       '("Living" "Deceased")))))
             (*group* (make-survey-group-named-and-numbered survey +survey-name-lam-history+ t :order (list q21)))
             (q21a (make-question "Date of last confirmation"
                                  :prompt-format prompt-format-colon
                                  :data-type :date))
             (subgroup21-living (make-survey-sub-group-named *group* nil :order (list q21a)))
             (q21b (make-question "Date of death" :data-type :date))
             (q21c
              (apply #'make-question "What was the cause of death"
                     :prompt-suffix "?"
                     (radio-options
                      (choices-breaks-alist
                       '("Respiratory failure" "Infection" "Pulmonary thromboembolism" "Cancer" "Other")))))
             (subgroup21-dead (make-survey-sub-group-named *group* nil :order (list q21b q21c)))
             (q21d (make-question "Please indicate type of infection if known" :data-type :string))
             (subgroup21-infection (make-survey-sub-group-named *group* nil :order (list q21d)))
             (q21e (make-question "Please indicate type of cancer if known" :data-type :string))
             (subgroup21-cancer (make-survey-sub-group-named *group* nil :order (list q21e))))
        ;; Group rules
        (add-rule *group* q21 "Living" subgroup21-living ':inline)
        (add-rule *group* q21 "Deceased" subgroup21-dead ':inline)
        (add-rule subgroup21-dead q21c "Infection" subgroup21-infection :inline)
        (add-rule subgroup21-dead q21c "Cancer" subgroup21-cancer :inline))

      ;;
      ;; Group 11 - results from other surveys
      ;;
      (let* ((q22
              (apply #'make-question "Do you have pulmonary function test (PFT) reports for the patient" :number 22.
                     :prompt-format prompt-format-numbered-question
                     (choices-options-yes-no)))
             (q22-confirm
              (apply #'make-question "Confirm that all PFT results have been entered for the patient"
                     :prompt-format "Please enter <B>all</B> the results that you have for the patient on the separate PFT patient diary.<BR>~A."
                     (choices-options-yes-no)))
             (q23
              (apply #'make-question "Does the patient have <B>six minute walk distance</B> (6MWD) results" :number 23.
                     :prompt-format prompt-format-numbered-question
                     (choices-options-yes-no)))
             (q23-confirm
              (apply #'make-question "Confirm that all 6MWD results have been entered for the patient"
                     :prompt-format "Please enter <B>all</B> the results that you have for the patient on the separate 6MWD patient diary.<BR>~A."
                     (choices-options-yes-no)))
             (q24
              (apply #'make-question "Has the patient taken the <B>St. George's Respiratory Questionnaire (SGRQ)</B>" :number 24.
                     :prompt-format prompt-format-numbered-question
                     (choices-options-yes-no)))
             (q24-confirm
              (apply #'make-question "Confirm that all SGRQ results have been entered for the patient"
                     :prompt-format "Please enter <B>all</B> the results that you have for the patient on the separate SGRQ patient diary.<BR>~A."
                     (choices-options-yes-no)))
             (*group* (make-survey-group-named-and-numbered survey +survey-name-lam-history+ t :order (list q22 q23 q24)))
             ;; Subgroups
             (subgroup22 (make-survey-sub-group-named *group* nil :order (list q22-confirm)))
             (subgroup23 (make-survey-sub-group-named *group* nil :order (list q23-confirm)))
             (subgroup24 (make-survey-sub-group-named *group* nil :order (list q24-confirm))))
        ;; Group rules
        (add-rule *group* q22 t subgroup22 ':inline)
        (add-rule *group* q23 t subgroup23 ':inline)
        (add-rule *group* q24 t subgroup24 ':inline))

      ;;
      ;; Survey - treatment/surgery
      ;;
      (let* ((q1 (make-question "Date of treatment/surgery" :prompt-suffix ":" :data-type :date))
             (*group*
              (make-survey-group-named-and-numbered survey-treatment "LAM History Treatment Diary" t))
             (q2
              (let* ((question
                      (apply #'make-question "Pneumothroax or pleural effusion treatment"
                             :prompt-suffix "<BR>Please specify:"
                             (radio-options
                              (choices-breaks-alist
                               '("Chest tube placement/pleural drainage"
                                 "Open chest surgery"
                                 "Thorascopic/minimally invasive chest surgery"
                                 "Pleurodesis"
                                 "Other")))))
                     (subq1 (make-question "Type of open chest surgery"))
                     (subg1
                      (let ((subgroup
                             (make-survey-sub-group-named *group* "open chest surgery subgroup" :order (list subq1))))
                        (add-rule *group* question "Open chest surgery" subgroup ':inline)
                        subgroup))
                     (subq2 (make-question "Type of thorascopic/minimally invasive chest surgery"))
                     (subg2
                      (let ((subgroup
                             (make-survey-sub-group-named *group* "thorascopic/minimally invasive chest surgery subgroup" :order (list subq2))))
                        (add-rule *group* question "Thorascopic/minimally invasive chest surgery" subgroup ':inline)
                        subgroup))
                     (subg3
                      (let* ((q1 (apply #'make-question "Pleurodesis side"
                                        (radio-options (choices-mirror-alist '("Left" "Right" "Both")))))
                             (subgroup
                              (make-survey-sub-group-named *group* "pleurodesis subgroup"))
                             (q2
                              (let* ((question
                                      (apply #'make-question "Type of pleurodesis"
                                             (radio-options (choices-mirror-alist '("Chemical" "Surgical/mechanical")))))
                                     (subquestion (make-question "Pleurodesis drug name"))
                                     (subgroup-drug (make-survey-sub-group-named subgroup "pleurodesis drug name subgroup" :order (list subquestion))))
                                (add-rule subgroup question "Chemical" subgroup-drug ':inline)
                                ;; Returns
                                question)))
                        ;; Group rules
                        (add-rule *group* question "Pleurodesis" subgroup ':inline)
                        (setf (group-questions subgroup) (list q1 q2))
                        ;; Returns
                        subgroup))
                     (subq4 (make-question "Other treatment - please specify"))
                     (subg4
                      (let ((subgroup
                             (make-survey-sub-group-named *group* "other treatment - specify" :order (list subq4))))
                        (add-rule *group* question "Other" subgroup ':inline)
                        subgroup)))

                (declare (ignore subg1 subg2 subg3 subg4))
                ;; Returns
                question)))
        ;; Survey properties
        (setf (group-questions *group*) (list q1 q2))
        (setf (diary-question survey-treatment) q1))

      ;;
      ;; Survey - PFT diary
      ;;
      (labels ((before-after-str (str &optional before)
                 (format nil "~A: ~A" str (if before "before" "after")))
               (make-pft-question-table (&key before advice)
                 ;; TBD: use before flag to munge the :NAME properties!!
                 (let ((table
                        (make-survey-group-table
                         (:name "pft results table" :default-question-args (:data-type :number))
                         ("Test" "Result" "Percent Predicted" "Test not performed")
                         ("Vital Capacity (VC)" 
                          (:question :name "VC result" :data-type :number :help "ml.") (:question :name "VC pct" :data-type :number :help "(%)")
                          (:question :name "VC tested" :data-type :boolean :view-type :checkbox))
                         ("Forced Vital Capacity (FVC)"
                          (:question :name "FVC result" :help "ml.") (:question :name "FVC pct" :help "(%)")
                          (:question :name "FVC tested" :data-type :boolean :view-type :checkbox))
                         ("Forced Expiratory Volume in 1 sec (FEV1)"
                          (:question :name "FEV1 result" :help "ml.") (:question :name "FEV1 pct" :help "(%)")
                          (:question :name "FEV1 tested" :data-type :boolean :view-type :checkbox))
                         ("Carbon Monoxide Diffusing Capacity (DLCO)"
                          (:question :name "DLCO result" :help "ml/min/mm Hg.") (:question :name "DLCO pct" :help "(%)")
                          (:question :name "DLCO tested" :data-type :boolean :view-type :checkbox))
                         ("DLCO/(alveolar volume)(V<SUB>A</SUB>))"
                          (:question :name "DLCO/Va result" :help"ml/min/mm Hg/L.") (:question :name "DLCO/Va pct" :help "(%)")
                          (:question :name "DLCO/Va tested" :data-type :boolean :view-type :checkbox))
                         ("Total Lung Capacity (TLC)"
                          (:question :name "TLC result" :help "ml.") (:question :name "TLC pct" :help "(%)")
                          (:question :name "TLC tested" :data-type :boolean :view-type :checkbox))
                         ("Residual Volume (RV)"
                          (:question :name "RV result" :help "ml.") (:question :name "RV pct" :help "(%)")
                          (:question :name "RV tested" :data-type :boolean :view-type :checkbox)))))
                   ;; Modify all question names based on before/after flag
                   (dolist (question (group-questions table))
                     (setf (question-name question)
                           (format nil "~A ~:[post~;pre~]-bronchodilator" (question-name question) before)))
                   ;; Group properties
                   (setf (group-advice table) advice)
                   (setf (owner table) owner)
                   ;; Returns
                   table)))
        (let* ((q1
                (make-question "Date of Pulmnonary Function Test" :data-type :date))
               (q2
                (apply #'make-question "Please check if the patient had any of the following at the time of this PFT result"
                       (multi-choices-options
                        (choices-breaks-alist
                         '("Pneumothorax" "Pleural effusion" "Pneumonia" "Chest surgery within the previous 6 months" "Unknown")))))
               (group1
                (make-survey-group-named-and-numbered survey-pft "LAM History PFT Diary" t :order (list q1 q2)))
               #+NIL
               (q3
                (apply #'make-question "Were tests performed <B>PRE-BRONCHODILATOR</B>"
                       :prompt-suffix " (examples: albuterol or ipratroprium inhaler/nebulizer)?"
                       (choices-options-yes-no)))
               ;; group 2 is a table of questions
               (group2
                (let ((table
                       (make-pft-question-table :before t :advice "These results are <B>PRE-BRONCHODILATOR</B> results:")))
                  (setf (group-name table)
                        (group-section-name-and-number survey-pft "LAM History PFT Diary" t))
                  (setf (survey-groups survey-pft) (append (survey-groups survey-pft) (list table)))
                  ;; Returns
                  table))
               ;; Second set, repeat of questions post-bronchodilator
               (q4
                (apply #'make-question "Were tests performed <B>POST-BRONCHODILATOR</B>"
                       :prompt-suffix " (examples: albuterol or ipratroprium inhaler/nebulizer)?"
                       (choices-options-yes-no)))
               (group3
                (make-survey-group-named-and-numbered survey-pft "LAM History PFT t" t :order (append (list q4))))
               (q4subgroup (make-pft-question-table :before nil)))
          (declare (ignore group1 group2))
          ;; Group rules
          (add-rule group3 q4 t q4subgroup ':successor)
          ;; Survey properties
          (setf (diary-question survey-pft) q1)))

      ;;
      ;; Survey - 6MWD diary
      ;;
      (let* ((q1 (make-question "Date" :data-type :date))
             (q2 (make-question "Result" :prompt-suffix " (feet/meters)" :data-type :number))
             (*group*
              (make-survey-group-named-and-numbered survey-6mwd "LAM History 6MWD" nil :order (list q1 q2))))
        (declare (ignore *group*))
        ;; Survey diary question
        (setf (diary-question survey-6mwd) q1))

      ;;
      ;; Survey - SGRQ diary
      ;;
      (let* ((q1 (make-question "Date" :data-type :date))
             (q2 (make-question "Total Score" :data-type :number))
             (q3 (make-question "Symptoms Score" :data-type :number))
             (q4 (make-question "Activity Score" :data-type :number))
             (q5 (make-question "Impacts Score" :data-type :number))
             (*group*
              (make-survey-group-named-and-numbered survey-sgrq "LAM History SGRQ" nil :order (list q1 q2 q3 q4 q5))))
        (declare (ignore *group*))
        ;; Survey diary question
        (setf (diary-question survey-sgrq) q1))

      ;; Returns
      (list survey survey-treatment survey-pft survey-6mwd survey-sgrq))))

(defun create-lam-history-data (&key (count 100.) (center "lamhtest"))
  (let ((questions (gethash +survey-name-lam-history+ *survey-question-table*)))
    (assert (not (null questions)) nil "Survey questions not found for ~S" +survey-name-lam-history+)
    (with-transaction ()
      ;; Coerce center
      (when (stringp center)
        (setq center
              (or (get-center center t)
                  (make-center center "LAM History survey - test center"))))
      (check-type center center)
      ;; Create / init test patients
      (mapcar 'drop-instance (get-patients-for-center center)))
    (with-transaction ()
      (let (
            ;; Random states for questions that define strata for our "sample"
            (q3-age-rs (make-random-state t))
            (q4-diagnosis-rs (make-random-state t))
            (q5-symptoms-rs (make-random-state t))
            (q6-symptoms-rs (make-random-state t))
            (q7-diagnosis-method-rs (make-random-state t))
            (q9-origin-rs (make-random-state t))
            (q20-rs (make-random-state t))
            )

      (dotimes (n count)
        (let* ((patient (make-patient (generate-patient-id :center center) center))
               age-now ;; see below
               (q3 (aref questions 3.)) ;age
               (q4 (aref questions 4.)) ;diagnosis
               (q5 (aref questions 5.)) ;TSC-LAM symptoms
               (q6 (aref questions 6.)) ;TSC-LAM organs affected
               (q7 (aref questions 7.)) ;How was LAM diagnosed?
               (q9 (aref questions 9.)) ;Original symptom or finding
               (q10 (aref questions 10.)) ;Age of first symptoms
               (q15 (aref questions 15.)) ;Age of menarche
               (q20 (aref questions 20.)) ;Use oxygen?
               )
          (setf (external-id patient) (symbol-name (gensym)))

          ;; Create test survey answers

          ;; Age-related answers
          ;; Fix this!! need to create a spread between the min and max possible ages
          (add-answer q3 patient (setq age-now (+ 18. (random 62. q3-age-rs))))
          (add-answer q10 patient (min age-now (+ 18. (random 22. q3-age-rs))))
          (add-answer q15 patient (min 13. age-now (+ 11. (random 5. q3-age-rs))))

          ;; Diagnosis answers
          (add-answer q4 patient
                      (let ((rnd (random 100. q4-diagnosis-rs)))
                        (cond
                          ((< rnd 2.) "Unknown")
                          ;; Diagnosis TSC-LAM
                          ((< rnd 12.)
                           ;; Symptoms
                           (add-answer q5 patient
                                       (let (answer)
                                         (block gather-symptoms
                                           (dolist (symptom (mapcar #'cdr (question-choices q5)))
                                             (when (= (random 3 q5-symptoms-rs) 1)
                                               (when (member symptom '("None" "Unknown") :test #'string-equal)
                                                 (setq answer (list symptom))
                                                 (return-from gather-symptoms))
                                               (push symptom answer))))
                                         answer))
                           ;; Organs affected
                           (add-answer q6 patient
                                       (let (answer)
                                         (block gather-symptoms
                                           (dolist (symptom (mapcar #'cdr (question-choices q6)))
                                             (when (= (random 3 q6-symptoms-rs) 1)
                                               (when (member symptom '("None" "Unknown/not screened") :test #'string-equal)
                                                 (setq answer (list symptom))
                                                 (return-from gather-symptoms))
                                               (push symptom answer))))
                                         answer))
                           "TSC-LAM")
                          (t "LAM"))))
          (add-answer q7 patient
                      (let (answer)
                        (block gather-symptoms
                          (dolist (symptom (mapcar #'cdr (question-choices q7)))
                            (when (= (random 6. q7-diagnosis-method-rs) 1)
                              (when (member symptom '("Unknown") :test #'string-equal)
                                (setq answer (list symptom))
                                (return-from gather-symptoms))
                              (push symptom answer))))
                        answer))
          (add-answer q9 patient
                      (let (answer)
                        (block gather-symptoms
                          (dolist (symptom (mapcar #'cdr (question-choices q9)))
                            (when (= (random 6. q9-origin-rs) 1)
                              (when (member symptom '("Unknown") :test #'string-equal)
                                (setq answer (list symptom))
                                (return-from gather-symptoms))
                              (push symptom answer))))
                        answer))
          (add-answer q20 patient (= (random 7. q20-rs) 1.))
                      
          ))))
    ;; Done
    ))

(defun create-lam-history-report (&key (questions '(3 4 5 6 7 9 10 15 20)) (center "lamhtest")
                                  (stream *standard-output*))
  (if (stringp center)
      (setq center (get-center center)))
  (let* ((qarray (gethash +survey-name-lam-history+ *survey-question-table*))
         (patients (get-patients-for-center center)))
    (format stream "~&Patients: ~D" (length patients))
    (dolist (patient patients)
      (format t "~&--- ~A" (id patient))
      (dolist (num questions)
        (let ((question (aref qarray num)))
          (format stream "~& >> ~A~&    << ~S"
                  (question-name question)
                  (let ((answer (get-answer question patient)))
                    (and answer (value answer)))))))))
    
