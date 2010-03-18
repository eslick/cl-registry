;; -*- Mode:Lisp; tab-width:2; indent-tabs-mode:nil -*-

;;; Copyright (c) 2008-2010, Massachusetts Institute of;Technology. All rights reserved. 
;;; Copyright (c) 2008-2010, LAM Treatment Alliance. All rights reserved. 
;;; Released under a BSD-style license: http://www.opensource.org/licenses/bsd-license.php 
;;; See LICENSE file 

(in-package :registry)

;;;
;;; Define LAM QOL/PFT surveys
;;; QOL/PFT == Quality of Life / Pulmonary Function Tests
;;;

;;; Globals

(defconstant +survey-name+ "LAM QOL/PFT")
(defconstant +survey-name-clinician+ "ILR QOL/PFT Clinician Questionnaire")
(defconstant +survey-short-name-clinician+ "QOL/PFT Study")
(defconstant +survey-name-patient+ "LAMsight QOL/PFT Patient Questionnaire")
(defconstant +survey-short-name-patient+ "QOL/PFT Study")

(defconstant +study-name-clinician+ "ILR QOL/PFT Study")

;;; Utilities

(defmacro prepend-survey-name (str survey)
  `(format nil "~A - ~A" ,survey ,str))

(defun make-pft-question-table (&key before advice (owner (current-user)))
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
    table))

;;; Create surveys

(defun create-ilr-qol/pft-surveys (&key (owner (current-user)))
  (with-transaction ()
    (let* ((prompt-format-colon "~A:")
           (prompt-format-question "~A?")
           (prompt-format-numbered-colon "~*<SUP>~D</SUP>~*&nbsp;~A:")
           (prompt-format-numbered-question "~*<SUP>~D</SUP>~*&nbsp;~A?")
           (survey-args `(:owner ,owner :origin "researcher" :published t :priority 1 :diary-p nil
                          :ranking-record (make-ranking-record :ranking nil :distribution nil)))
           (diary-args `(:owner ,owner :origin "researcher" :published t :priority 2 :diary-p t
                         :ranking-record (make-ranking-record :ranking nil :distribution nil)))
           (survey-clinician
            (apply #'make-instance 'survey
                   :name +survey-name-clinician+
                   :description "This study involves a retrospective medical record review, with a particular focus on pulmonary function tests."
                   survey-args))
           (survey-treatment
            (apply #'make-instance 'survey
                   :name (prepend-survey-name "Pneumothorax or Pulmonary Effusion Treatment Diary" +survey-name-clinician+)
                   :description "Enter information about surgery/treatments for LAM patient."
                   :diary-description "One result per date"
                   diary-args))
           (survey-pft
            (apply #'make-instance 'survey
                   :name (prepend-survey-name "Pulmonary Function Test (PFT) Diary" +survey-name-clinician+)
                   :description "Enter PFT results for LAM patient."
                   :diary-description "One result per date"
                   diary-args))
           (survey-6mwd
            (apply #'make-instance 'survey
                   :name (prepend-survey-name "Six Minute Walking Distance (6MWD) Diary" +survey-name-clinician+)
                   :description "Enter 6MWD results for LAM patient."
                   :diary-description "One result per date"
                   diary-args))
           (survey-sgrq
            (apply #'make-instance 'survey
                   :name (prepend-survey-name "Saint George's Respiratory Questionnaire (SGRQ) Diary" +survey-name-clinician+)
                   :description "Enter SGRQ results for LAM patient."
                   :diary-description "One result per date"
                   diary-args)))

      ;;
      ;; Main survey - ILR QOL/PFT Clinician Questionnaire
      ;;

      ;;
      ;; Group 1 - clinician header page
      ;;
      (make-survey-group-named-and-numbered survey-clinician +survey-short-name-clinician+ t
                                            :advice "<B>ILR Clinician Data Entry Form Header</B>
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
             (*group*
              (make-survey-group-named-and-numbered survey-clinician +survey-short-name-clinician+ t
                                                    :order (list q1 q2 q3 q4)))
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
      (let* ((*group*
              (make-survey-group-named-and-numbered survey-clinician +survey-short-name-clinician+ t))
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
        ;; Questions
        (setf (group-questions *group*) (list q7)))

      ;;
      ;; Group 4
      ;;
      (let* ((*group*
              (make-survey-group-named-and-numbered survey-clinician +survey-short-name-clinician+ t
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
        ;; Questions
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
              (make-survey-group-named-and-numbered survey-clinician +survey-short-name-clinician+ t
                                                    :order (list q9 q10 q11 q12 q13)))
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
             (*group* (make-survey-group-named-and-numbered survey-clinician +survey-short-name-clinician+ t
                                                            :order (list q14)))
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
                                       ( nil "Number <B>before</B> diagnosis of LAM"
                                             "Number <B>during</B> or <B>after</B> diagnosis of LAM" )
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
              (make-survey-group-named-and-numbered survey-clinician +survey-short-name-clinician+ t
                                                    :order (list q15 q16 q17 q18)))
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
              (make-survey-group-named-and-numbered survey-clinician +survey-short-name-clinician+ t
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
             (bronch/meds/question
              (apply #'make-question "Bronchodilator/Pulmonary medications" (choices-options-yes-no)))
             (bronch/meds/table
              (make-survey-group-table
               (:name "bronchodilator/pulmonary medications table" :default-question-args (:data-type :date))
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
        (add-rule *group* bronch/meds/question t bronch/meds/table ':inline)
        (add-rule *group* other/meds/question t other/med/table ':inline)
        (add-rule *group* other/surgery/question t other/surgery/table ':inline)
        ;; Questions
        (setf (group-questions *group*)
              (list hormone-therapy/question bronch/meds/question other/meds/question
                    pneumothorax/pleural-effusion/question other/surgery/question
                    transplant/question)))

      ;;
      ;; Group 9
      ;;
      (let* ((q20
              (apply #'make-question "Does/did the patient use oxygen at home" :number 20.
                     :prompt-format prompt-format-numbered-question
                     (choices-options-yes-no)))
             (*group*
              (make-survey-group-named-and-numbered survey-clinician +survey-short-name-clinician+ t
                                                            :order (list q20)))
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
             (*group*
              (make-survey-group-named-and-numbered survey-clinician +survey-short-name-clinician+ t
                                                    :order (list q21)))
             (q21a (make-question "Date of last confirmation"
                                  :prompt-format prompt-format-colon
                                  :data-type :date))
             (subgroup21-living (make-survey-sub-group-named *group* nil :order (list q21a)))
             (q21b (make-question "Date of death" :data-type :date))
             (q21c
              (apply #'make-question "What was the cause of death"
                     :prompt-format prompt-format-question
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
              (apply #'make-question "Do you have at least two (2) pulmonary function test (PFT) reports for the patient from different dates" :number 22.
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
             (*group*
              (make-survey-group-named-and-numbered survey-clinician +survey-short-name-clinician+ t
                                                    :order (list q22 q23 q24)))
             ;; Subgroups
             (subgroup22yes (make-survey-sub-group-named *group* nil :order (list q22-confirm)))
             (subgroup22no (make-survey-sub-group-named *group* nil :order nil
                                                        :advice "This patient is not eligible for this study."))
             (subgroup23yes (make-survey-sub-group-named *group* nil :order (list q23-confirm)))
             (subgroup24yes (make-survey-sub-group-named *group* nil :order (list q24-confirm))))
        ;; Group rules
        (add-rule *group* q22 t subgroup22yes ':inline)
        (add-rule *group* q22 nil subgroup22no ':inline)
        (add-rule *group* q23 t subgroup23yes ':inline)
        (add-rule *group* q24 t subgroup24yes ':inline))

      ;;
      ;; Survey - treatment/surgery
      ;;
      (let* ((q1 (make-question "Date of treatment/surgery" :prompt-format prompt-format-colon :data-type :date))
             (*group*
              (make-survey-group-named-and-numbered survey-treatment (prepend-survey-name "Treatment Diary" +survey-short-name-clinician+) t))
             (q2
              (let* ((question
                      (apply #'make-question "Pneumothroax or pleural effusion treatment"
                             :prompt-format prompt-format-colon
                             (radio-options
                              (choices-breaks-alist
                               '("Chest tube placement/pleural drainage"
                                 "Open chest surgery"
                                 "Thorascopic/minimally invasive chest surgery"
                                 "Pleurodesis"
                                 "Other")))))
                     (subq1 (make-question "Type of open chest surgery"
                                           :prompt-format prompt-format-colon))
                     (subg1
                      (let ((subgroup
                             (make-survey-sub-group-named *group* "open chest surgery subgroup" :order (list subq1))))
                        (add-rule *group* question "Open chest surgery" subgroup ':inline)
                        subgroup))
                     (subq2 (make-question "Type of thorascopic/minimally invasive chest surgery"
                                           :prompt-format prompt-format-colon))
                     (subg2
                      (let ((subgroup
                             (make-survey-sub-group-named *group* "thorascopic/minimally invasive chest surgery subgroup" :order (list subq2))))
                        (add-rule *group* question "Thorascopic/minimally invasive chest surgery" subgroup ':inline)
                        subgroup))
                     (subg3
                      (let* ((q1 (apply #'make-question "Pleurodesis side"
                                        :prompt-format prompt-format-colon
                                        (radio-options (choices-mirror-alist '("Left" "Right" "Both")))))
                             (subgroup
                              (make-survey-sub-group-named *group* "pleurodesis subgroup"))
                             (q2
                              (let* ((question
                                      (apply #'make-question "Type of pleurodesis"
                                             :prompt-format prompt-format-colon
                                             (radio-options (choices-mirror-alist '("Chemical" "Surgical/mechanical")))))
                                     (subquestion (make-question "Pleurodesis drug name"
                                                                 :prompt-format prompt-format-colon))
                                     (subgroup-drug (make-survey-sub-group-named subgroup "pleurodesis drug name subgroup" :order (list subquestion))))
                                (add-rule subgroup question "Chemical" subgroup-drug ':inline)
                                ;; Returns
                                question)))
                        ;; Group rules
                        (add-rule *group* question "Pleurodesis" subgroup ':inline)
                        (setf (group-questions subgroup) (list q1 q2))
                        ;; Returns
                        subgroup))
                     (subq4 (make-question "Other treatment - please specify"
                                           :prompt-format prompt-format-colon))
                     (subg4
                      (let ((subgroup
                             (make-survey-sub-group-named *group* "other treatment - specify" :order (list subq4))))
                        (add-rule *group* question "Other" subgroup ':inline)
                        subgroup)))

                (declare (ignore subg1 subg2 subg3 subg4))
                ;; Returns
                question)))
        ;; Questions
        (setf (group-questions *group*) (list q1 q2))
        ;; Survey properties
        (setf (diary-question survey-treatment) q1))

      ;;
      ;; Survey - PFT diary
      ;;
      (let* ((q1
              (make-question "Date of Pulmonary Function Test"
                             :prompt-format prompt-format-colon :data-type :date))
             (q2
              (apply #'make-question "Please check if the patient had any of the following at the time of this PFT result"
                     :prompt-format prompt-format-colon
                     (multi-choices-options
                      (choices-breaks-alist
                       '("Pneumothorax" "Pleural effusion" "Pneumonia" "Chest surgery within the previous 6 months" "Unknown")))))
             (group1
              (make-survey-group-named-and-numbered survey-pft (prepend-survey-name "PFT Diary" +survey-short-name-clinician+) t :order (list q1 q2)))
             ;; Group 2 is a table of questions
             (group2
              (let ((table
                     (make-pft-question-table :before t
                                              :owner owner
                                              :advice "These results are <B>PRE-BRONCHODILATOR</B> results:")))
                (setf (group-name table)
                      (group-section-name-and-number survey-pft (prepend-survey-name "PFT Diary" +survey-short-name-clinician+) t))
                (setf (survey-groups survey-pft) (append (survey-groups survey-pft) (list table)))
                ;; Returns
                table))
             ;; Second set, repeat of questions post-bronchodilator
             (q4
              (apply #'make-question "Were tests performed <B>POST-BRONCHODILATOR</B>"
                     :prompt-format "~A (examples: albuterol or ipratroprium inhaler/nebulizer)?"
                     (choices-options-yes-no)))
             (group3
              (make-survey-group-named-and-numbered survey-pft (prepend-survey-name "PFT Diary" +survey-short-name-clinician+) t :order (append (list q4))))
             (q4subgroup (make-pft-question-table :before nil :owner owner)))
        (declare (ignore group1 group2))
        ;; Group rules
        (add-rule group3 q4 t q4subgroup ':successor)
        ;; Survey properties
        (setf (diary-question survey-pft) q1))

      ;;
      ;; Survey - 6MWD diary
      ;;
      (let* ((q1 (make-question "Date" :prompt-format prompt-format-colon :data-type :date))
             (q2 (make-question "Result" :prompt-format prompt-format-colon :help "(feet/meters)" :data-type :number))
             (*group*
              (make-survey-group-named-and-numbered survey-6mwd (prepend-survey-name"6MWD" +survey-short-name-clinician+) nil :order (list q1 q2))))
        (declare (ignore *group*))
        ;; Survey diary question
        (setf (diary-question survey-6mwd) q1))

      ;;
      ;; Survey - SGRQ diary
      ;;
      (let* ((q1 (make-question "Date" :prompt-format prompt-format-colon :data-type :date))
             (q2 (make-question "Total Score" :prompt-format prompt-format-colon :data-type :number))
             (q3 (make-question "Symptoms Score" :prompt-format prompt-format-colon :data-type :number))
             (q4 (make-question "Activity Score" :prompt-format prompt-format-colon :data-type :number))
             (q5 (make-question "Impacts Score" :prompt-format prompt-format-colon :data-type :number))
             (*group*
              (make-survey-group-named-and-numbered survey-sgrq (prepend-survey-name "SGRQ" +survey-short-name-clinician+) nil :order (list q1 q2 q3 q4 q5))))
        (declare (ignore *group*))
        ;; Survey diary question
        (setf (diary-question survey-sgrq) q1))

      ;; Returns
      `( (,survey-clinician :DOFIRST)
         (,survey-pft :REQUIRED)
         (,survey-treatment :OPTIONAL)
         (,survey-6mwd :OPTIONAL)
         (,survey-sgrq :OPTIONAL))
      )))

(defun create-lamsight-qol/pft-surveys (&key (owner (current-user)))
  (with-transaction ()
    (let* ((prompt-format-colon "~A:")
           (prompt-format-question "~A?")
           (prompt-format-numbered-colon "~*<SUP>~D</SUP>~*&nbsp;~A:")
           (prompt-format-numbered-question "~*<SUP>~D</SUP>~*&nbsp;~A?")
           (survey-args `(:owner ,owner :origin "researcher" :published t :priority 1 :diary-p nil
                          :ranking-record (make-ranking-record :ranking nil :distribution nil)))
           (survey-patient
            (apply #'make-instance 'survey
                   :name +survey-name-patient+
                   :description "This study involves a retrospective medical record review, with a particular focus on pulmonary function tests."
                   survey-args)))

      ;;
      ;; Main survey - LAMsight QOL/PFTPatient Questionnaire
      ;;

      ;;
      ;; Group 1 - Patient header page
      ;;
      (make-survey-group-named-and-numbered survey-patient +survey-short-name-clinician+ t
                                            :advice "<B>LAMsight QOL/PFT Data Entry Form Header</B>
<P>Thank you for agreeing to take part in this research study! We hope to use this information to answer questions that are important to you and the LAM community.
<P>We expect that the following questionnaires will take you about 20-30 minutes. You do not need to complete all of the questions in one setting, however we do ask that you complete two of the surveys (the SF-36 and SGRQ) within one week of each other. You may skip any question that you are uncomfortable answering. If any of the questions are unclear, please feel free to contact
<A HREF=\"mailto:LAM.research.study@gmail.com\">LAM.research.study@gmail.com</A> with comments or questions.
<P>You will need copies of your pulmonary function test (PFT) reports to complete this study. You may need to contact your health care provider to obtain these reports. In the last part of this study, we will ask you to enter your most recent PFT results on an online collection form. Finally, we will ask you to submit to us all of your previous pulmonary function test (PFT) results, including your most recent report. Your pulmonary function results can be entered and submitted at any time.")

      ;;
      ;; Group 2 - Patient info
      ;;
      (let* ((*group* (make-survey-group-named-and-numbered survey-patient +survey-short-name-patient+ t))
             (q1 (make-question "Name" :number 1. :prompt "Name (First, Last)" :prompt-format prompt-format-numbered-colon))
             (q2 (make-question "Date of birth" :number 2. :prompt-format prompt-format-numbered-colon))
             (q3 (make-question "What country do you live in" :number 3. :prompt-format prompt-format-numbered-colon))
             (q4 (make-question "How many total years of schooling or education have you completed"
                                :number 4. 
                                :prompt-format prompt-format-numbered-question :data-type :number))
             (q5
              (let* ((question
                      (apply #'make-question "Are you completing this survey in a language other than your primary language"
                             :number 5. 
                             :prompt-format prompt-format-numbered-question
                             (radio-options
                              (choices-mirror-alist '("Yes" "No" "I don't know")))))
                     (q/lang
                      (make-question "what is your primary language"
                                     :prompt-format "If yes ~A?" ))
                     (q/years
                      (make-question "how many years of education have you had in English"
                                     :prompt-format "If yes ~A?" :data-type :number))
                     (subgroup
                      (make-survey-sub-group-named *group* "language questions" :order (list q/lang q/years))))
                (add-rule *group* question "Yes" subgroup ':inline)
                ;; Returns
                question))
             (q6 (make-question "What is your height"
                                :number 6.
                                :prompt-format prompt-format-numbered-question
                                :data-type :measurement
                                :data-subtype :length))
             (q7 (make-question "What is your weight"
                                :number 7.
                                :prompt-format prompt-format-numbered-question
                                :data-type :measurement
                                :data-subtype :weight))
             (q8 (apply #'make-question "What is your ethnicity"
                        :number 8.
                        :prompt-format prompt-format-numbered-question
                        (radio-options
                         (choices-breaks-alist '(("Hispanic or Latino" . t) ("Not Hispanic or Latino" . nil))))))
             (q9
              (let* ((question
                      (apply #'make-question "What is your race"
                             :number 9.
                             :prompt-format prompt-format-numbered-question
                             (radio-options
                              (choices-breaks-alist
                               '("American Indian/Alaska Native"
                                 "Asian" "Native Hawaiian or Other Pacific Islander"
                                 "Black or African American"
                                 "White" "Other")))))
                     (q/other (make-question "Other race" :prompt-format prompt-format-colon))
                     (subgroup
                      (make-survey-sub-group-named *group* "race questions - other" :order (list q/other))))
                (add-rule *group* question "Other" subgroup ':inline)
                ;; Returns
                question))
             (q10
              (let* ((question
                      (apply #'make-question "What is your marital status"
                             :number 10.
                             :prompt-format prompt-format-colon
                             (radio-options
                              (choices-breaks-alist
                               '("Single" "Married" "Separated" "Divorced" "Widowed" "Other")))))
                     (q/other (make-question "Other marital status" :prompt-format prompt-format-colon))
                     (subgroup
                      (make-survey-sub-group-named *group* "marital status questions - other" :order (list q/other))))
                (add-rule *group* question "Other" subgroup ':inline)
                ;; Returns
                question)))
        ;; Questions
        (setf (group-questions *group*) (list q1 q2 q3 q4 q5 q6 q7 q8 q9 q10)))
        
      ;; Group 3 - Diagnosis
      ;;
      (let* ((*group*
              (make-survey-group-named-and-numbered survey-patient +survey-short-name-patient+ t))
             (q11
              (apply #'make-question "Diagnosis type"
                     :number 11.
                     :prompt "I was diagnosed with"                     
                     :prompt-format prompt-format-numbered-colon
                     (radio-options
                      '(("Sporadic LAM" . "LAM")
                        ("Tuberous Sclerosis Complex with LAM (TSC-LAM)" . "TSC-LAM")
                        ("I don't know" . "Unknown")))))
             ;; Rule: if q11 answer is TSC-LAM...
             (q12
              (apply #'make-question  "If you have TSC-LAM, what signs of TSC do you have"
                     :number 12.
                     :prompt-format prompt-format-numbered-question
                     (multi-choices-options
                      (choices-mirror-alist
                       '("Skin" "Brain" "Kidney" "Eye" "Other")))))
             ;; Rule: if previous answer included Other...
             (q12o
              (make-question "Other TSC-LAM symptoms" :prompt-format prompt-format-colon
                                                      :data-type :string :view-type :text-field))

             (q13
              (make-question "Date of diagnosis of LAM or TSC-LAM"
                             :number 13.
                             :prompt-format prompt-format-numbered-colon
                             :data-type :date))
             (q14
              (make-question "When did you first have LAM related symptoms"
                             :number 14.
                             :prompt-format prompt-format-numbered-colon
                             :data-type :date))
             (q15
              (let* ((question
                      (apply #'make-question "What were your main symptoms that led to your diagnosis of LAM"
                             :number 15.
                             :prompt-format prompt-format-numbered-question
                             (radio-options
                              (choices-breaks-alist
                               '("Shortness of breath" "Chest pain" "Abdominal pain" "Coughing up blood"
                                 "I did not have symptoms" "Other")))))
                     (q/other (make-question "Other symptoms"))
                     (subgroup (make-survey-sub-group-named *group* "symptoms - other" :order (list q/other))))
                (add-rule *group* question "Other" subgroup ':inline)
                ;; Returns
                question))
             (q16
              (let* ((question
                      (apply #'make-question "How were you diagnosed with LAM"
                             :number 16.
                             :prompt-format prompt-format-numbered-question
                             (radio-options
                              (choices-breaks-alist
                               '("By lung biopsy" "By biopsy of lymph node or other mass" "By CT (CAT) scan or imaging"
                                 "Other" "I don't know")))))
                     (q/other (make-question "Other diagnosis"))
                     (subgroup (make-survey-sub-group-named *group* "diagnosis - other" :order (list q/other))))
                (add-rule *group* question "Other" subgroup ':inline)
                ;; Returns
                question))
             ;; Subgroups
             (subgroup1 (make-survey-sub-group-named *group* nil :order (list q12)))
             (subgroup2 (make-survey-sub-group-named *group* nil :order (list q12o))))
        ;; Group rules
        (add-rule *group* q11 "TSC-LAM" subgroup1 ':successor)
        (add-rule subgroup1 q12 "Other" subgroup2 ':successor)
        ;; Questions
        (setf (group-questions *group*) (list q11 q13 q14 q15 q16)))
      
      ;;
      ;; Group 4 - Problems and symptoms
      ;;
      (let* ((*group*
              (make-survey-group-named-and-numbered survey-patient +survey-short-name-patient+ t))
             (q17
              (let* ((question
                      (apply #'make-question "Do you smoke or did you smoke in the past"
                             :number 17.
                             :prompt-format prompt-format-numbered-question
                             (radio-options
                              (choices-breaks-alist '("Yes I smoke" "I quit" "I never smoked")))))
                     (table/yes
                      (make-survey-group-table
                       (:name "smoking? yes table" :default-question-args (:data-type :number))
                       (nil nil nil)
                       ("On average I smoke" (:question :name "cigarettes per day") "cigarettes per day")
                       ("How long have you been smoking?" (:question :name "smoking years" ) "years")))
                     (table/quit
                      (make-survey-group-table
                       (:name "smoking? quit table" :default-question-args (:data-type :number))
                       (nil nil nil)
                       ("On average I used to smoke" (:question :name "cigarettes per day before quit") "cigarettes per day")
                       ("I smoked for?" (:question :name "smoked years before quit" ) "years before I quit")
                       ("I quit" (:question :name "quit months past") "months/years ago"))))
                (add-rule *group* question "Yes I smoke" table/yes ':inline)
                (add-rule *group* question "I quit" table/quit ':inline)
                ;; Returns
                question))
             (q18
              (let* ((question
                      (apply #'make-question "Have you ever had a pneumothorax"
                             :number 18.
                             :prompt-format (concatenate 'string
                                                         prompt-format-numbered-question
                                                         " A pneumothorax is air between the lung and chest wall,
which can cause a collapsed lung.")
                             (radio-options
                              (choices-mirror-alist '("Yes" "No" "I don't know")))))
                     (q/how-many-ever
                      (make-question "how many pneumothoraces ever"
                                     :prompt "If yes how many?" :data-type :number))
                     (q/how-many-ever-past-year
                      (make-question "how many pneumothoraces past year"
                                     :prompt "If yes how pneumothoraces have you had in the past year?"
                                     :data-type :number))
                     (subgroup (make-survey-sub-group-named *group* "pneumothoraces subgroup"
                                                            :order (list q/how-many-ever q/how-many-ever-past-year))))
                (add-rule *group* question "Yes" subgroup ':inline)
                ;; Returns
                question))
             (q19
              (apply #'make-question "Have you ever had a pleural effusion"
                     :number 19.
                     :prompt-format (concatenate 'string
                                                 prompt-format-numbered-question
                                                 " A pleural effusion is a fluid collection between the lung and chest wall that sometimes requires drainage.")
                     (radio-options
                      (choices-mirror-alist '("Yes" "No" "I don't know")))))
             (q20
              (apply #'make-question "Have you ever had abdominal fluid or ascites"
                     :number 20.
                     :prompt-format prompt-format-numbered-question
                     (radio-options
                      (choices-mirror-alist '("Yes" "No" "I don't know"))))))
        ;; Questions
        (setf (group-questions *group*) (list q17 q18 q19 q20)))

      ;;
      ;; Group 5
      ;; 
      (let* ((*group*
              (make-survey-group-named-and-numbered survey-patient +survey-short-name-patient+ t))
             (q21
              (let* ((question
                      (apply #'make-question "Have you ever had a pleurodesis"
                             :number 21.
                             :prompt-format (concatenate 'string
                                                         prompt-format-numbered-question
                                                         " This is a procedure to make your lung adhere to your chest wall to treat a pleural effusion or pneumothorax.")
                             (radio-options
                              (choices-mirror-alist '("Yes" "No" "I don't know")))))
                     (q/how-many
                      (make-question "If yes how many pleurodeses"
                                     :prompt-format prompt-format-question
                                     :data-type :number))
                     (q/which-lungs
                      (apply #'make-question "pleurodesis which lungs"
                             :prompt-format ""
                             (radio-options
                              (choices-mirror-alist '("One lung" "Both lungs")))))
                     (q/how-often
                      (make-question "If yes how many times have you had a pleurodesis in the last year"
                                     :prompt-format prompt-format-question
                                     :data-type :number))
                     (subgroup
                      (make-survey-sub-group-named *group* "pleurodesis subgroup" :order (list q/how-many q/which-lungs q/how-often))))
                ;; Group rules
                (add-rule *group* question "Yes" subgroup ':inline)
                ;; Returns
                question))
             (q22
              (let* ((question
                      (apply #'make-question "Have you ever had other surgical treatments for a pneumothorax or pleural effusion"
                             :number 22.
                             :prompt-format prompt-format-numbered-question
                             (radio-options
                              (choices-mirror-alist '("Yes" "No" "I don't know")))))
                     (q/treatments
                      (apply #'make-question "Other treatments"
                             :prompt-format ""
                             (radio-options
                              (choices-breaks-alist
                               '("I had a chest tube placed" "I had a thoracoscopic or minimally invasive procedure"
                                 "I had open chest surgery" "Other")))))
                     (subgroup
                      (make-survey-sub-group-named *group* "treatments subgroup" :order (list q/treatments))))
                ;; Group rules
                (add-rule *group* question "Yes" subgroup ':inline)
                ;; Returns
                question))
             (q23
              (let* ((question
                      (apply #'make-question "Have you had a lung transplant"
                             :number 23.
                             :prompt-format prompt-format-numbered-question
                             (choices-options-yes-no)))
                     (q/which-lungs
                      (apply #'make-question "transplant which lungs" :prompt-format ""
                             (radio-options '(("One lung" . 1) ("Both lungs" . 2)))))
                     (q/date (make-question "Transplant date" :data-type :date))
                     (subgroup
                      (make-survey-sub-group-named *group* "transplant question subgroup"
                                                   :order (list q/which-lungs q/date))))
                ;; Group rules
                (add-rule *group* question t subgroup ':inline)
                ;; Returns
                question)))
        ;; Questions
        (setf (group-questions *group*) (list q21 q22 q23)))

      ;;
      ;; Group 6
      ;;
      (let* ((*group*
              (make-survey-group-named-and-numbered survey-patient +survey-short-name-patient+ t))
             (q24
              (let* ((subgroup
                      (make-survey-sub-group-named *group* "q24 subgroup"))
                     (question
                      (apply #'make-question "Do you use oxygen at home"
                             :number 24.
                             :prompt-format prompt-format-numbered-question
                             (choices-options-yes-no)))
                     (q/how
                      (let* ((question
                              (apply #'make-question "If yes the patient uses oxygen at home"
                                     :prompt "If yes:"
                                     (multi-choices-options
                                      (choices-mirror-alist
                                       '("Continuous" "With activities" "At night" "Other")))))
                             (q/other (make-question "uses oxygen - other"))
                             (subgroup2 (make-survey-sub-group-named subgroup nil :order (list q/other))))
                        ;; Group rules
                        (add-rule subgroup question "Other" subgroup2 ':inline)
                        ;; Returns
                        question))
                     (q/how-much
                      (make-question "Average #liters/min"
                                     :prompt-format prompt-format-colon
                                     :data-type :number))
                     (q/how-long
                      (make-question "When did the patient initiate home oxygen"
                                     :prompt-format prompt-format-question
                                     :data-type :date)))
                ;; Questions
                (setf (group-questions subgroup) (list q/how q/how-much q/how-long))
                ;; Group rules
                (add-rule *group* question t subgroup ':inline)
                ;; Returns
                question))
             (q25
              (apply #'make-question "Do you have kidney disease (for example, angiomyolipomas (AMLs))"
                     :number 25.
                     :prompt-format prompt-format-numbered-question
                     (radio-options
                      (choices-mirror-alist '("Yes" "No" "I don't know")))))
             (q26
              (apply #'make-question "What <B>best</B> describes your level of breathlessness during activity"
                     :number 26.
                     :prompt-format prompt-format-numbered-question
                     (radio-options
                      (choices-breaks-alist
                       '(("I do not have breathlessness during activities of daily living" . 0)
                         ("I get breathless with strenuous exercise" . 1)
                         ("I get short of breath when hurrying on level ground or walking up a slight hill" . 2)
                         ("I walk slower than people of the same age on level ground or stop for breath while walking at my own pace on level ground" . 3)
                         ("I stop for breath after walking 100 yards (100 meters)" . 4)
                         ("I am too breathless to leave the house, or become breathless when dressing or undressing" . 5)))))))
        ;; Questions
        (setf (group-questions *group*) (list q24 q25 q26)))

      ;;
      ;; Group 7
      ;;
      (let* ((*group*
              (make-survey-group-named-and-numbered survey-patient +survey-short-name-patient+ t))
             (q27
              (let* ((question
                      (apply #'make-question "Did you take hormonal contraception (birth control pills) before you were diagnosed with LAM"
                             :number 27.
                             :prompt-format prompt-format-numbered-question
                             (radio-options
                              (choices-mirror-alist '("Yes" "No" "I don't know")))))
                     (q/type
                      (make-question "Please specify type"
                                     :prompt-format prompt-format-colon
                                     :data-type :string))
                     (q/date
                      (make-question "Dates of use"
                                     :prompt-format prompt-format-colon
                                     :data-type :date-range))
                     (subgroup
                      (make-survey-sub-group-named *group* "q16" :order (list q/type q/date))))
                ;; Group rules
                (add-rule *group* question "Yes" subgroup ':inline)
                ;; Returns
                question))
             (q28
              (let* ((question
                      (apply #'make-question "Have you ever been or are you currently pregnant"
                             :number 28.
                             :prompt-format prompt-format-numbered-question
                             (radio-options
                              (choices-mirror-alist '("Yes" "No" "I don't know")))))
                     (table28
                      (make-survey-group-table (:name "question27 if yes" :advice "If yes: please complete:" :default-question-args (:data-type :number))
                                       ( nil "Number <B>before</B> your diagnosis of LAM"
                                             "Number <B>during</B> or <B>after</b> your diagnosis of LAM" )
                                       ( "Full term or premature births" (:question) (:question) )
                                       ( "Miscarriages"  (:question) (:question) )
                                       ( "Abortions" (:question) (:question) ))))
                ;; Group rules
                (add-rule *group* question "Yes" table28 ':successor)
                ;; Returns
                question))
             (q29
              (let* ((question
                      (apply #'make-question "Have you gone through menopause"
                             :number 29.
                             :prompt-format prompt-format-numbered-question
                             (radio-options
                              (choices-mirror-alist '("Yes" "No" "I don't know")))))
                     (q/age
                      (make-question "Age at time of menopause"
                                     :prompt "If yes, age at time of menopause?"
                                     :help "(years)" :data-type :number))
                     (subgroup
                      (make-survey-sub-group-named *group* "q28 subgroup" :order (list q/age))))
                ;; Group rules
                (add-rule *group* question "Yes" subgroup ':inline)
                ;; Returns
                question)))
        ;; Questions
        (setf (group-questions *group*) (list q27 q28 q29)))

      ;;
      ;; Group 8 - treatments
      ;;
      (flet ((make-table-current-past-use (prefix)
               (let ((table
                      (make-survey-group-table
                       (:name "current or past use table" :default-question-args (:data-type :date))
                       (nil nil nil nil nil)
                       ((:question :name "current use answer" :data-type :boolean :view-type :checkbox)
                        "Current use."
                        "When did you start?" (:question :name "Start date" :data-type :date) 
                        nil nil)
                       ((:question :name "past use answer" :data-type :boolean :view-type :checkbox)
                        "Past use."
                        "Start date:" (:question :name "Start date" :data-type :date)
                        "End date:" (:question :name "End date" :data-type :date)))))
                 ;; Modify all question names based on before/after flag
                 (dolist (question (group-questions table))
                   (setf (question-name question)
                         (format nil "~A - ~A" prefix (question-name question))))
                 ;; Returns
                 table)))
        (let* ((*group*
                (make-survey-group-named-and-numbered survey-patient +survey-short-name-patient+ t
                                                      :advice "<SUP>30</SUP> What LAM related <B>treatments</B> have you had or are you currently using? <SMALL>Please check all that apply</SMALL>"))
               (none/question
                (apply #'make-question "None" (choices-options-yes-no)))
               (bronch/meds/question
                (let* ((question
                        (apply #'make-question "Bronchodilator therapy"
                               :prompt "Bronchodilators (Examples: albuterol, ipratroprium inhalers or nebulizers)"
                               (choices-options-yes-no)))
                       (table (make-table-current-past-use "Bronchodilator")))
                  ;; Group rules
                  (add-rule *group* question t table ':inline)
                  ;; Returns
                  question))
               (steroids/meds/question
                (let* ((question
                        (apply #'make-question "Inhaled steroids therapy"
                               :prompt "Inhaled steroids"
                               (choices-options-yes-no)))
                       (table (make-table-current-past-use "Inhaled steroids")))
                  ;; Group rules
                  (add-rule *group* question t table ':inline)
                  ;; Returns
                  question))
               (hormone-therapy/question
                (let* ((question
                        (apply #'make-question "Hormone therapy"
                               :prompt "Anti-estrogen (hormonal) medical therapy (Examples: progesterone pills or injection, GnRH, Tamoxifen)"
                               (choices-options-yes-no)))
                       (table (make-table-current-past-use "Hormones")))
                  ;; Group rules
                  (add-rule *group* question t table ':inline)
                  ;; Returns
                  question))
               (sirolimus/rapamune/question
                (let* ((question
                        (apply #'make-question "Sirolimus/Rapamune therapy"
                               :prompt "Sirolimus/Rapamune"
                               (choices-options-yes-no)))
                       (table (make-table-current-past-use "Sirolimus/Rapamune")))
                  ;; Group rules
                  (add-rule *group* question t table ':inline)
                  ;; Returns
                  question))
               (ovaries/question
                (let* ((question
                        (apply #'make-question "I have had my ovaries removed"
                               (choices-options-yes-no)))
                       (q/date (make-question "Date ovaries removed"
                                              :prompt "Date:"
                                              :data-type :date))
                       (q/therapy
                        (apply #'make-question "If yes do/did you use hormone replacement therapy"
                               :prompt-format prompt-format-question
                               (radio-options
                                (choices-mirror-alist '("Yes" "No" "I don't know")))))
                       (subgroup (make-survey-sub-group-named *group* "ovaries removed subgroup" :order (list q/date q/therapy))))
                  ;; Group rules
                  (add-rule *group* question t subgroup ':inline)
                  ;; Returns
                  question))
               (other/question
                (let* ((question
                        (apply #'make-question "Other therapy" :prompt "Other:"
                               (choices-options-yes-no)))
                       (q/other (make-question "Other therapy - please specify"
                                               :prompt-format prompt-format-colon))
                       (subgroup (make-survey-sub-group-named *group* "other therapy subgroup" :order (list q/other))))
                  ;; Group rules
                  (add-rule *group* question t subgroup ':inline)
                  ;; Returns
                  question)))
          ;; Questions
          (setf (group-questions *group*)
                (list none/question bronch/meds/question steroids/meds/question
                      hormone-therapy/question sirolimus/rapamune/question ovaries/question
                      other/question))))

      ;;
      ;; Groups 9, 10, 11 - PFT
      (let* ((*group*
              (make-survey-group-named-and-numbered survey-patient +survey-short-name-patient+ t
                                                    :advice "You will need a copy of your <B>most recent</B> pulmonary function test report to complete this part of the study. You may need to contact your health care provider to get a copy of this report."))
             (q1
              (make-question "Date of your most recent Pulmonary Function Test"
                             :prompt "Date of your <B>most recent</B> Pulmonary Function Test:"
                             :data-type :date))
             (q2
              (make-question "Check here if you have not had pulmonary function tests performed in the last 5 years."
                             :data-type :boolean :view-type :checkbox))
             (q3
              (apply #'make-question "Did you have any of the following at the time of this PFT result"
                       :prompt-format prompt-format-question
                       (multi-choices-options
                        (choices-breaks-alist
                         '("Pneumothorax" "Pleural Effusion" "Pneumonia or respiratory infection"
                           "Chest Surgery within the previous 6 months"
                           "I don't know")))))
             ;; Group 2 is a table of questions
             (group2
              (let ((table
                     (make-pft-question-table :before t
                                              :owner owner
                                              :advice "These results are <B>PRE-BRONCHODILATOR</B> results:")))
                (setf (group-name table)
                      (group-section-name-and-number survey-patient +survey-short-name-patient+ t))
                  (setf (survey-groups survey-patient) (append (survey-groups survey-patient) (list table)))
                  ;; Returns
                  table))
             ;; Second set, repeat of questions post-bronchodilator
             (q4
              (apply #'make-question "Did you have tests performed <B>POST-BRONCHODILATOR</B>"
                     :prompt-format "~A (examples: albuterol or ipratroprium inhaler/nebulizer)?"
                     (radio-options
                      (choices-mirror-alist '("Yes" "No" "I don't know")))))
             (group3
              (make-survey-group-named-and-numbered survey-patient +survey-short-name-patient+ t
                                                    :order (append (list q4))))
             (q4subgroup (make-pft-question-table :before nil :owner owner)))
          (declare (ignore group2))
          ;; Questions
          (setf (group-questions *group*) (list q1 q2 q3))
          ;; Group rules
          (add-rule group3 q4 "Yes"  q4subgroup ':successor)
          ;; Survey properties
          (setf (diary-question survey-patient) q1))

      ;; 
      ;; Group 12 - Conclusion
      ;;
      (let ((*group*
             (make-survey-group-named-and-numbered survey-patient +survey-short-name-patient+ t
                                                   :advice "<P>Thank you for completing this form!
<P>Please submit a copy of all of your pulmonary function test reports including your most recent PFT report (the same report that you used to enter information on this site for this study).
<P>You may do this in any of the following ways:
<P>Web drop box:Click here to upload file 
<P>Email: <A HREF=\"mailto:LAM.research.study@gmail.com\">LAM.research.study@gmail.com</A>
<P>Fax: (001) 617-380-0046 Please use a cover sheet for your fax!
<P>Mail:
<PRE>
  Sarah Billmeier, MD
  Brigham and Women’s Hospital
  Surgical Education Office
  75 Francis St
  Boston, MA 02130  USA
</PRE>
<P>This will allow us to show that your entry of your pulmonary function test information is accurate enough to use in future research studies and to see how your quality of life is affected by your rate of change of pulmonary function.
<P>Thank you for sending in your pulmonary function reports!"
                                                   )))
        (declare (ignore *group*)))

      ;; Returns
      (list survey-patient))))

;;; Create studies

(defun create-ilr-qol/pft-study (&key (owner (current-user)))
  (let ((survey-rule-alist (create-ilr-qol/pft-surveys :owner owner))
        (study
         (make-instance 'study :name +study-name-clinician+
                               :description "Young / old patient study"
                               :published t :owner owner :priority 1 :origin "researcher")))
    (loop for spec in survey-rule-alist
         with surveys 
         with rules
         do (let ((survey (first spec))
                  (rule-type (second spec)))
              (push survey surveys)
              (push (make-survey-rule :survey survey :type rule-type) rules))
         finally
         (setf (surveys study) (reverse surveys) (survey-rules study) rules))
    ;; Returns
    study))

;;; Data analysis and reporting

(defun get-lam-qol/pft-questions (survey)
  ;; Coerce survey
  (unless (typep survey 'survey)
    (setq survey (or (get-survey survey) (error "Survey not found: ~A" survey))))
  (let* ((last-question 30.)
         (questions (make-array (1+ last-question) :element-type 'question :adjustable t)))
    ;; Cache numbered questions
    (with-transaction ()
      (let* ((question-count 0.)
             (groups
              (loop for group in (survey-groups survey)
                   append (find-subgroups group))))
        (dolist (group groups)
          (dolist (question (group-questions group))
            (when  (slot-boundp question 'number)
              (let ((num (question-number question)))
                (when (typep num '(integer 1))
                  (setf last-question (max num last-question))
                  (when (> last-question (1+ (length questions)))
                    (adjust-array questions (+ last-question 8.)))
                  (setf (aref questions num) question)
                  (incf question-count))))))
        ;; Returns
        (values questions question-count)))))

(defun create-lam-qol/pft-data (&key (count 100.) (center "lamhtest"))
 (let ((survey +survey-name-clinician+))
  (multiple-value-bind (questions question-count) (get-lam-qol/pft-questions survey)

    (unless (plusp question-count)
      (error "Survey questions not found for ~S" survey))

    ;; Create or initialize center
    (with-transaction ()
      ;; Coerce center
      (when (stringp center)
        (setq center
              (or (get-center center t)
                  (make-center center (prepend-survey-name "test center" survey)))))
      (check-type center center)
      ;; Delete any existing patients
      (mapcar 'drop-instance (get-patients-for-center center)))

    ;; Create test data
    (with-transaction ()
      (let (
            ;; Random states for questions that define strata for our "sample"
            (q3-age-rs (make-random-state t))
            (q3-age-group-rs (make-random-state t))
            (q4-diagnosis-rs (make-random-state t))
            (q5-symptoms-rs (make-random-state t))
            (q6-symptoms-rs (make-random-state t))
            (q7-diagnosis-method-rs (make-random-state t))
            (q9-origin-rs (make-random-state t))
            (q10-age-pct-rs (make-random-state t))
            (q20-rs (make-random-state t))
            )
        
      (dotimes (n count)
        (let* ((patient (make-patient (generate-patient-id :center center) center))
               patient-age ;; see below
               (q3 (aref questions 3.)) ;age at diagnosis
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
          ;; Create a (more or less) bi-modal distribution for age at diagnosis young vs. old
          ;; First flip a coin to assign this patient to one or the other group
          (add-answer q3 patient
                      (setq patient-age
                            (floor 
                             (if (= (random 2. q3-age-group-rs) 1.) ;flip a coin
                                 ;; Age 15 - 25
                                 (statistics:random-normal :mean 25. :sd 5.)
                                 ;; Age 55 - 75
                                 (statistics:random-normal :mean 55. :sd 5.)))))
          ;; Age of first symptoms: range from 50% - 100% of age of diagnosis
          (add-answer q10 patient (floor (* patient-age (/ (1+ (random 100. q10-age-pct-rs)) 100.))))
          ;; Age of menarche: range from 12 - age of diagnosis
          (add-answer q15 patient (min patient-age (+ 12. (random 5. q3-age-rs))))

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
    )))

(defun create-lam-qol/pft-report (&key survey (center "lamhtest")
                                  (questions '(3 4 5 6 7 9 10 15 20))
                                  (stream *standard-output*))
  (assert (not (null survey)))
  (if (stringp center)
      (setq center (get-center center)))
  (let* ((qarray (get-lam-qol/pft-questions survey))
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
    
