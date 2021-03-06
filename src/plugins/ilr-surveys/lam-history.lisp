;; -*- Mode:Lisp; tab-width:2; indent-tabs-mode:nil -*-

;;; Copyright (c) 2008-2010, Massachusetts Institute of;Technology. All rights reserved. 
;;; Copyright (c) 2008-2010, LAM Treatment Alliance. All rights reserved. 
;;; Released under a BSD-style license: http://www.opensource.org/licenses/bsd-license.php 
;;; See LICENSE file 

(in-package :registry)

;;;
;;; Define LAM studies and surveys
;;;  ILR-ARR-PFT: ILR (clinician) age related rate of PFT decline study 
;;;  LAMS-QOL-PF: LAMsight (patient) quality of life and pulmonary function study

;;; Globals

(defconstant +study-name-clinician+ "Age related rate of PFT decline study")

(defconstant +study-name-patient+ "Quality of life and pulmonary function study")

(defconstant +survey-name-introduction+ "Background and history")

(defconstant +survey-name-clinician+ (survey-name-append +study-name-clinician+ +survey-name-introduction+))

(defconstant +survey-name-patient+ (survey-name-append +study-name-patient+ +survey-name-introduction+))

;;; Utilities

(defun drop-ilr-surveys (surveys)
  (format t "~%Dropping ~D surveys starting with groups..." (length surveys))
  (dolist (survey surveys)
    (dolist (group (survey-groups survey))
      (and group (drop-group group :interactive nil)))
    (drop-instance survey)))

(defun drop-ilr-studies (&key force (interactive (not force)))
  (cond
    ((and force
          (or (not interactive)
              (yes-or-no-p
               "Are you sure you want to force delete all studies, surveys, groups, questions, and answers?")))
     (with-transaction ()
       (format t "~%Force dropping all surveys, answers, questions...")
       (mapcar 'drop-instance (get-instances-by-class 'answer))
       (mapcar 'drop-instance (get-instances-by-class 'question))
       (mapcar 'drop-instance (get-instances-by-class 'survey-group))
       (mapcar 'drop-instance (get-instances-by-class 'survey))
       (mapcar 'drop-instance (get-instances-by-class 'study))))
    (t
     (flet ((drop-ilr-study (name)
              (awhen (get-instance-by-value 'study 'name name)
                (format t "~%Dropping study ~A starting with surveys..." name)
                (with-transaction ()
                  (drop-ilr-surveys (surveys it))
                  (drop-study it :interactive nil)
                  (drop-instance it)))))
       (drop-ilr-study +study-name-clinician+)
       (drop-ilr-study +study-name-patient+)))))

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

(defun create-ilr-arr/pft-surveys (&key (owner (current-user)) (study-name +study-name-clinician+))
  (with-transaction ()
    (let* ((prompt-format-colon "~A:")
           (prompt-format-question "~A?")
           (prompt-format-numbered-colon "~*<SUP>~D</SUP>~*&nbsp;~A:")
           (prompt-format-numbered-question "~*<SUP>~D</SUP>~*&nbsp;~A?")
           (survey-args `(:owner ,owner :origin "researcher" :published t :priority 1 :diary-p nil
                          :ranking-record (make-ranking-record :ranking nil :distribution nil)))
           (diary-args `(:owner ,owner :origin "researcher" :published t :priority 2 :diary-p t
                         :ranking-record (make-ranking-record :ranking nil :distribution nil)))
           (survey-intro
            (apply #'make-instance 'survey
                   :name "Introduction" ;; (survey-name-append study-name "Introduction")
                   :description #!"Cover sheet and confirm PFT results are in hand for patient"
                   survey-args))
           (survey-clinician
            (apply #'make-instance 'survey
                   :name +survey-name-clinician+
                   :description #!"Enter LAM patient's demographics, symptoms, method of diagnosis, and medical treatment history"
                   :formats '(:group-number-style :roman :question-number-style :decimal)
                   survey-args))
           (survey-treatment
            (apply #'make-instance 'survey
                   :name "Pneumothorax and Pleural Effusion Treatment Diary" ;; (survey-name-append study-name "Pneumothorax and Pleural Effusion Treatment Diary")
                   :description #!"If the patient had multiple
                   treatments, please create a separate diary entry
                   for each treatment. (Click 'Add Entry' to add a new
                   entry)"
                   :diary-description "Treatment diary entry"
                   diary-args))
           (survey-pft
            (apply #'make-instance 'survey
                   :name "Pulmonary Function Test (PFT) Diary" ;; (survey-name-append study-name "Pulmonary Function Test (PFT) Diary")
                   :description #!"Please enter all the results that you have for the patient, with one diary entry per PFT result. Use 'Add Entry' to enter multiple PFT results."
                   :diary-description "PFT results for date"
                   diary-args))
           (survey-sgrq
            (apply #'make-instance 'survey
                   :name "Saint George's Respiratory Questionnaire (SGRQ) Diary" ;; (survey-name-append study-name "Saint George's Respiratory Questionnaire (SGRQ) Diary")
                   :description #!"If more than one SGRQ test, please complete one diary entry per SGRQ result. Use 'Add Entry' to enter multiple results."
                   :diary-description #!"SGRQ results for date"
                   diary-args))
           (survey-6mwd
            (apply #'make-instance 'survey
                   :name "Six Minute Walking Distance (6MWD) Diary"  ;; (survey-name-append study-name "Six Minute Walking Distance (6MWD) Diary")
                   :description "If more than one 6MWD test, please complete one diary entry per 6MWD result. Use 'Add Entry' to enter multiple results."
                   :diary-description #!"6MWD results for date"
                   diary-args)))

      ;;
      ;; Introduction survey
      ;;
      (let* ((group1
              ;; Group 1 - clinician header page
              (make-survey-group-named survey-intro #!"Introduction"
                                       :advice "Thank you for your involvement in the LAM International Registry.
<P>Our first research study using the registry data will be to examine whether women diagnosed with LAM under age 25 have a more rapid pulmonary function decline relative to those over the age of 55.
<P>This study will analyze data from LAM patients who have had at least two pulmonary function test (PFT) reports from different dates and who are
<UL>
<LI>diagnosed with LAM at age 25 or under
<LI>diagnosed with LAM at age 55 or older
</UL>
<P>This study involves a retrospective medical record review with a particular focus on pulmonary function test.  In completing the following form, we ask that you comply with your local laws and regulations regarding access and use of patient records, and obtain consent whenever indicated.
Data identifiers such as name and date of birth will not be used, and each record will be given a unique identifier for the purpose of data storage.
For further information about data use please see 
<A HREF=\"/docs/ILR/lrb.2009.0028.lowlink.pdf_v03.pdf\">Nurok et al, International LAM Registry, in press, Lymphatic Research and Cell Biology Volume 8, Number 1, 2010</A>
<P>For this study, please complete the following data entry form for <B>current</B> and <B>former</B> LAM patients in your care.
<P>The time it takes to complete the following data entry form will vary, depending on the type of medical records you are using and the number of pulmonary function tests to enter.  
You may save your work at any point to complete at a later time.
<P> Additionally, please encourage all of your LAM patients to register on LAMsight.org!"))
             (group2
              (make-survey-group-named survey-intro #!"PFT results for patient"))
             (q/pft
              ;; If not 2 or more PFTs for patient, then display advice and link to home page
              (let* ((question
                      (apply #'make-question "Do you have at least two (2) pulmonary function test (PFT) reports for the patient from different dates?" 
                             :parent group2
                             (radio-options
                              (choices-mirror-alist '("Yes" "No"))))))
                ;; Returns
                question)))
        (declare (ignore group1))
        ;; Group questions
        (setf (group-questions group2) (list q/pft)))

      ;;
      ;; Main survey - ILR QOL/PFT Clinician Questionnaire
      ;;

      ;;
      ;; Patient info
      ;;
      (let* ((*group*
              (make-survey-group-named survey-clinician #!"Demographics & diagnosis"))
             (q1
              (make-question "Country where patient receives medical care" :number 1.
                             :prompt-format prompt-format-numbered-colon
                             :parent *group*
                             :data-type :string))
             (q2
              (make-question "Date of diagnosis of LAM or TSC-LAM" :number 2.
                             :prompt-format prompt-format-numbered-colon
                             :parent *group*
                             :data-type :date :data-subtype :date-month-year))
             (q3
              (make-question "Age at time of diagnosis" :number 3.
                             :prompt-format prompt-format-numbered-colon
                             :help "(years)"
                             :parent *group*
                             :data-type :number))
             (q4
              (apply #'make-question "Diagnosis type" :number 4.
                     :prompt-format prompt-format-numbered-colon
                     :parent *group*
                     (radio-options
                      '(("Sporadic LAM" . "LAM")
                        ("Tuberous Sclerosis Complex and LAM (TSC-LAM)" . "TSC-LAM")
                        ("Unknown" . "Unknown")))))
             ;; Rule: if q4 answer is TSC-LAM...
             (q5
              (apply #'make-question "If the patient has TSC-LAM, what symptoms does the patient have" :number 5.
                     :prompt-format prompt-format-numbered-question
                     :parent *group*
                     (multi-choices-options
                      (choices-mirror-alist
                       '("Developmental delay" "Behavioral problem" "Seizures" "Other" "None" "Unknown")))))
             ;; Rule: if q5 answer included Other...
             (q5o
              (make-question "Other TSC-LAM symptoms"
                             :parent *group*
                             :data-type :string :view-type :text-field))
             ;; Rule: if q4 answer is TSC-LAM...
             (q6
              (apply #'make-question "If the patient has TSC-LAM, what organ systems are affected by tumors" :number 6.
                     :prompt-format prompt-format-numbered-question
                     :parent *group*
                     (multi-choices-options
                      (choices-mirror-alist
                       '("Brain" "Kidneys" "Heart" "Eyes" "Skin" "Lungs" "Other" "Unknown/Not screened")))))
             ;; Rule: if previous answer included Other...
             (q6o
              (make-question "Other TSC-LAM organs effect by tumors" :data-type :string :view-type :text-field))
             ;; Subgroups
             (subgroup1 (make-survey-sub-group-named *group* nil :order (list q5 q6)))
             (subgroup2 (make-survey-sub-group-named *group* nil :order (list q5o)))
             (subgroup3 (make-survey-sub-group-named *group* nil :order (list q6o))))
        ;; Group
        (setf (group-questions *group*) (list q1 q2 q3 q4))
        (add-rule *group* q4 "TSC-LAM" subgroup1 ':successor)
        (add-rule subgroup1 q5 "Other" subgroup2 ':successor)
        (add-rule subgroup1 q6 "Other" subgroup3 ':successor))

      ;;
      ;; How was LAM diagnosed?
      ;;
      (let* ((*group*
              (make-survey-group-named survey-clinician #!"Diagnosis"))
             (q7
              (let* ((question-diagnosis
                      (apply #'make-question "How was LAM diagnosed" :number 7.
                             :prompt-format prompt-format-numbered-question
                             :parent *group*
                             (radio-options
                              (choices-breaks-alist
                               '("Lung biopsy" ; see group rule below
                                 "CT thorax and other tissue biopsy" ;see group rule below
                                 "CT thorax and AML" "CT thorax and chylous collection"
                                 "CT thorax and TSC" "CT thorax and VEGF-D >800"
                                 "Other" ;see group rule below
                                 "Unknown/not screened")))))
                     (subgroup-lung-biopsy
                      (make-survey-sub-group-named *group* "diagnosis lung biopsy subgroup"))
                     (question-biopsy-type
                      (let ((question
                             (apply #'make-question "If lung biopsy, please specify type"
                                    (radio-options
                                     (choices-breaks-alist
                                      '(("Open lung biopsy" . "open")
                                        ("Video-assisted thoracoscopic surgery (VATS) biopsy" . "VATS")
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
                      (make-question "Other - Please specify")))
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
      ;; Histopathology results4
      ;;
      (let* ((*group*
              (make-survey-group-named survey-clinician #!"Histopathology results"
                                       :advice "<SUP>8</SUP> If a biopsy was performed, what were the histopathological findings?"))
             (*questions*
              (loop for spec in
                   '(("HMB-45 immunostaining" "Positive" "Negative" "Not performed" "Unknown")
                     ("Estrogen receptors" "Positive" "Negative" "Not performed" "Unknown")
                     ("Progesterone receptors" "Positive" "Negative" "Not performed" "Unknown")
                     ("Gene mutation analysis" "TSC 1 mutation" "TSC 2 mutation" "Other" "Not performed" "Unknown"))
                   as name = (first spec)
                   as options = (rest spec)
                   collect
                   (apply #'make-question name
                          :parent *group*
                          (radio-options (choices-breaks-alist options)))))
             (subgroup-other
              (make-survey-sub-group-named *group* "gene mutation other subgroup"))
             (question-other
              (make-question "Other Gene Mutation")))
        ;; Questions
        (setf (group-questions subgroup-other) (list question-other))
        (setf (group-questions *group*) *questions*)
        (add-rule *group* (last1 *questions*) "Other" subgroup-other ':inline))

      ;;
      ;; Symptoms and complications
      ;;
      (let* ((*group*
              (make-survey-group-named survey-clinician #!"Symptoms and complications"))
             (q9
              (apply #'make-question "How did the patient originally present" :number 9.
                     :prompt-format (concatenate 'string prompt-format-numbered-question
                                                 " What symptom, finding or event led to the eventual diagnosis of LAM?")
                     :parent *group*
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
                             :parent *group*
                             :help "(years)" :data-type :number))
             (q11
              (apply #'make-question "What is the patient's smoking history" :number 11.
                     :prompt-format prompt-format-numbered-question
                     :parent *group*
                     (radio-options
                      (choices-breaks-alist
                       '("Current smoker" "Former smoker" "Never a smoker" "Unknown")))))
             (q11-current-packs
              (make-question "On average the patient smokes how many packs per day"
                             :prompt-format prompt-format-question
                             :data-type :number))
             (q11-current-years
              (make-question "How many years has the patient smoked"
                             :prompt-format prompt-format-question
                             :data-type :number))
             (q11-current-pack-years
              (make-question "Total pack years"
                             :prompt "<B>Or</B> enter total pack years:"
                             :data-type :number))
             (q11-former-packs
              (make-question "On average the patient smoked how many packs per day"
                             :prompt-format prompt-format-question
                             :data-type :number))
             (q11-former-years
              (make-question "How many years did the patient smoke"
                             :prompt-format prompt-format-question
                             :data-type :number))
             (q11-former-pack-years
              (make-question "Total pack years"
                             :prompt "<B>Or</B> enter total pack years:"
                             :data-type :number))
             (q12
              (apply #'make-question "Has the patient ever had a pneumothorax" :number 12.
                     :prompt-format prompt-format-numbered-question
                     :parent *group*
                     (radio-options
                      (choices-mirror-alist
                       '("Yes" "No" "Unknown")))))
             (q12a
              (make-question "If yes how many total"
                             :prompt-format prompt-format-question :data-type :number))
             (q12b
              (make-question "If yes, how many in the last year"
                             :prompt-format prompt-format-question :data-type :number))
             (q13
              (apply #'make-question "Has the patient ever had a pleural effusion" :number 13.
                     :prompt-format prompt-format-numbered-question
                     :parent *group*
                     (radio-options
                      (choices-mirror-alist
                       '("Yes" "No" "Unknown")))))
             ;; Subgroups
             (subgroup9o (make-survey-sub-group-named *group* nil :order (list q9o)))
             (subgroup11-current
              (make-survey-sub-group-named *group* nil
                                           :order (list q11-current-packs q11-current-years q11-current-pack-years)))
             (subgroup11-former
              (make-survey-sub-group-named *group* nil
                                           :order (list q11-former-packs q11-former-years q11-former-pack-years)))
             (subgroup12 (make-survey-sub-group-named *group* nil :order (list q12a q12b))))
        ;; Group rules
        (setf (group-questions *group*) (list q9 q10 q11 q12 q13))
        (add-rule *group* q9 "Other" subgroup9o ':inline)
        (add-rule *group* q11 "Current smoker" subgroup11-current ':inline)
        (add-rule *group* q11 "Former smoker" subgroup11-former ':inline)
        (add-rule *group* q12 "Yes" subgroup12 ':inline))

      ;;
      ;; Extrapulmonary lesions
      ;;
      (let* ((*group* (make-survey-group-named survey-clinician #!"Extrapulmonary lesions"))
             (q14
              (apply #'make-question "What extrapulmonary lesions does the patient have" :number 14.
                     :prompt-format prompt-format-numbered-question
                     :parent *group*
                     (multi-choices-options
                      (choices-mirror-alist
                       '("Renal angiomyolipoma" "Non-renal angiomyolipoma" "Lymphangioleiomyoma"
                         "Chylous ascites" "Chylous pleural effusion"
                         "Other" "None")))))
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
        (setf (group-questions *group*) (list q14))
        (add-rule *group* q14 "Renal angiomyolipoma" subgroup14a ':inline)
        (add-rule *group* q14 "Non-renal angiomyolipoma" subgroup14b ':inline)
        (add-rule subgroup14b q14b "Other" subgroup14b2 ':inline)
        (add-rule *group* q14 "Chylous pleural effusion" subgroup14c ':inline)
        (add-rule *group* q14 "Other" subgroup14o ':inline))

      ;;
      ;; Estrogen exposure history
      ;;
      (let* ((*group*
              (make-survey-group-named survey-clinician #!"Estrogen exposure history"))
             (q15
              (make-question "What was the patient's age of menarche?"
                             :prompt-format prompt-format-numbered-question
                             :parent *group*
                             :number 15.
                             :help "Leave blank and click below if unknown"
                             :data-type :number))
             (q15a 
              (make-question "Unknown"
                             :parent *group*
                             :number 15.1
                             :data-type :boolean
                             :view-type :checkbox))
             (q16
              (apply #'make-question "Did the patient take hormonal contraception prior to the diagnosis of LAM" :number 16.
                     :prompt-format prompt-format-numbered-question
                     :parent *group*
                     (radio-options
                      (choices-mirror-alist
                       '("Yes" "No" "Unknown")))))
             (q16a
              (make-question "Please specify type of contraceptive" :data-type :string))
             (q16b
              (make-question "Dates of use" :data-type :date-range :data-subtype :date-month-year
                             :help "Use 'present' for end date when appropriate"))
             (q17
              (apply #'make-question "Has the patient ever been pregnant" :number 17.
                     :prompt-format prompt-format-numbered-question
                     :parent *group*
                     (radio-options
                      (choices-mirror-alist
                       '("Yes" "No" "Unknown")))))
             (q17-table
              (make-survey-group-table (:name "question 17 if yes" :advice "If yes: please complete:" :default-question-args (:data-type :number))
                                       ( nil "Number <B>before</B> diagnosis of LAM"
                                             "Unknown"
                                             "Number <B>during</B> or <B>after</B> diagnosis of LAM"
                                             "Unknown")
                                       ( "Number of pregnancies" (:question) 
                                                                 (:question :name "number of pregnancies before unknown"
                                                                            :data-type :boolean :view-type :checkbox)
                                                                 (:question)
                                                                 (:question :name "number of pregnancies during/after unknown"
                                                                            :data-type :boolean :view-type :checkbox))
                                       ( "Number of births" (:question) 
                                                            (:question :name "number of births before unknown"
                                                                       :data-type :boolean :view-type :checkbox)
                                                            (:question)
                                                            (:question :name "number of births during/after unknown"
                                                                       :data-type :boolean :view-type :checkbox))))

             (q18
              (apply #'make-question "Has the patient gone through menopause" :number 18.
                     :prompt-format prompt-format-numbered-question
                     :parent *group*
                     (radio-options
                      (choices-mirror-alist
                       '("Yes" "No" "Unknown")))))
             (q18a
              (make-question "Age at time of menopause"
                             :prompt "If yes, age at time of menopause?"
                             :help "(years)" :data-type :number))
             ;; Subgroups
             (subgroup16
              (make-survey-sub-group-named *group* "Dates of contraceptive use." :order (list q16a q16b)))
             (subgroup18 (make-survey-sub-group-named *group* nil :order (list q18a))))
        ;; Group rules
        (setf (group-questions *group*) (list q15 q15a q16 q17 q18))
        (add-rule *group* q16 "Yes" subgroup16 ':inline)
        (add-rule *group* q17 "Yes" q17-table ':inline)
        (add-rule *group* q18 "Yes" subgroup18 ':inline))

      ;;
      ;; Treatment history
      ;;
      (let* ((*group*
              (make-survey-group-named survey-clinician #!"Treatment history"
                                       :advice "<SUP>19</SUP> What is the patient's LAM related <B>treatment</B> history? <BR><SMALL>Please check all that apply.<BR>You may use 'present' for end date where appropriate</SMALL>"))
             (no-treatment 
              (make-question "No Treatment"
                             :parent *group*
                             :data-type :boolean 
                             :view-type :checkbox
                             :help "Check if no treatments taken, leave the rest of the page blank"))
             (hormone-therapy/question
              (apply #'make-question "Hormone therapy"
                     :parent *group*
                     (choices-options-yes-no)))
             (hormone-therapy/table
              (make-survey-group-table
               (:name "hormone therapy table"
                :default-question-args (:data-type :date :data-subtype :date-month-year))
               (nil "Treatment" nil "Start date" "End date" nil)
               ((:question :name "hormone therapy: Gn-RH agonist" :data-type :boolean :view-type :checkbox )
                "Gn-RH agonist" nil
                (:question :name "hormone therapy: Gn-RH agonist: from") (:question :name "hormone therapy: Gn-RH agonist: to") "<small><i>mm/yy</i></small>")
               ((:question :name "hormone therapy: Progesterone" :data-type :boolean :view-type :checkbox )
                "Progesterone" nil
                (:question :name "hormone therapy: Progesterone: from") (:question :name "hormone therapy: Progesterone: to") "<small><i>mm/yy</i></small>")
               ((:question :name "hormone therapy: Tamoxifen" :data-type :boolean :view-type :checkbox )
                "Tamoxifen" nil
                (:question :name "hormone therapy: Tamoxifen: from") (:question :name "hormone therapy: Tamoxifen: to") "<small><i>mm/yy</i></small>")
               ((:question :name "hormone therapy: surgical oophorectomy" :data-type :boolean :view-type :checkbox )
                "Surgical oophorectomy" nil
                (:question :name "hormone therapy: surgical oophorectomy: date") "<small><i>mm/dd/yy</i></small>" nil)
               ((:question :name "hormone therapy: other" :data-type :boolean :view-type :checkbox )
                "Other" (:question :data-type :string)
                (:question :name "hormone therapy: other: from") (:question :name "hormone therapy: other: to") "<small><i>mm/yy</i></small>")))
             (bronch/meds/question
              (apply #'make-question "Bronchodilator/Pulmonary medications"
                     :parent *group*
                     (choices-options-yes-no)))
             (bronch/meds/table
              (make-survey-group-table
               (:name "bronchodilator/pulmonary medications table"
                :default-question-args (:data-type :date :data-subtype :date-month-year))
               (nil "Treatment" nil "Start Date" "End Date" nil)
               ((:question :name "bronch/pulm meds therapy: long acting Beta agonist" :data-type :boolean :view-type :checkbox )
                "Long acting Beta agonist" nil (:question) (:question) "<small><i>mm/yy</i></small>")
               ((:question :name "bronch/pulm meds therapy: oral Beta agonist" :data-type :boolean :view-type :checkbox )
                "Oral Beta agonist" nil
                (:question :name "bronch/pulm meds therapy: oral Beta agonist: from") (:question :name "bronch/pulm meds therapy: oral Beta agonist: to") "<small><i>mm/yy</i></small>")
               ((:question :name "bronch/pulm meds therapy: transdermal Beta agonist" :data-type :boolean :view-type :checkbox )
                "Transdermal Beta agonist" nil
                (:question :name "bronch/pulm meds therapy: transdermal Beta agonist: from") (:question :name "bronch/pulm meds therapy: transdermal Beta agonist: to") "<small><i>mm/yy</i></small>")
               ((:question :name "bronch/pulm meds therapy: anti-cholinergic" :data-type :boolean :view-type :checkbox )
                "Anti-cholinergic" nil
                (:question :name "bronch/pulm meds therapy: anti-cholinergic: from") (:question :name "bronch/pulm meds therapy: anti-cholinergic: to") "<small><i>mm/yy</i></small>")
               ((:question :name "bronch/pulm meds therapy: Aminophylline" :data-type :boolean :view-type :checkbox )
                "Aminophylline" nil
                (:question :name "bronch/pulm meds therapy: Aminophylline: from") (:question :name "bronch/pulm meds therapy: Aminophylline: to") "<small><i>mm/yy</i></small>")
               ((:question :name "bronch/pulm meds therapy: inhaled steroids" :data-type :boolean :view-type :checkbox )
                "Inhaled steroids" nil
                (:question :name "bronch/pulm meds therapy: inhaled steroids: from") (:question :name "bronch/pulm meds therapy: inhaled steroids: to") "<small><i>mm/yy</i></small>")
               ((:question :name "bronch/pulm meds therapy: other" :data-type :boolean :view-type :checkbox )
                "Other" (:question :data-type :string)
                (:question :name "bronch/pulm meds therapy: other: from") (:question :name "bronch/pulm meds therapy: other: to") "<small><i>mm/yy</i></small>")))
             (other/meds/question
              (apply #'make-question "Other medical treatment"
                     :parent *group*
                     (choices-options-yes-no)))
             (other/med/table
              (make-survey-group-table
               (:name "other medical treatment table"
                :default-question-args (:data-type :date :data-subtype :date-month-year))
               (nil "Treatment" nil "Start Date" "End Date" nil)
               ((:question :name "other medical treatment: Sirolimus/Rapamune" :data-type :boolean :view-type :checkbox )
                "Sirolimus/Rapamune" nil
                (:question :name "other medical treatment: Sirolimus/Rapamune: from") (:question :name "other medical treatment: Sirolimus/Rapamune: to") "<small><i>mm/yy</i></small>")
               ((:question :name "other medical treatment: other" :data-type :boolean :view-type :checkbox )
                "Other" (:question :name "other medical treatment: other: specify" :data-type :string)
                (:question :name "other medical treatment: other: from") (:question :name "other medical treatment: other: to") "<small><i>mm/yy</i></small>")))
             (pneumothorax/pleural-effusion/question
              (let* ((question
                      (apply #'make-question "Pneumothorax or pleural effusion treatment"
                             :parent *group*
                             (choices-options-yes-no)))
                     (subgroup
                      (make-survey-sub-group-named
                       *group* "pneumothorax or pleural effusion treatment advice subgroup"
                       :advice "Please fill out the Pneumothorax and Pleural Effusion Treatment Diary under the Collect tab")))
                (add-rule *group* question "Yes" subgroup ':inline)
                ;; Returns
                question))
             (other/surgery/question
              (apply #'make-question "Other surgery"
                     :parent *group*
                     (choices-options-yes-no)))
             (other/surgery/table
              (make-survey-group-table
               (:name "other surgery table"
                :default-question-args (:data-type :date :data-subtype :date-month-year))
               (nil "Surgery" nil "Date" nil)
               ((:question :name "other surgery: thoracic duct ligation" :data-type :boolean :view-type :checkbox )
                "Thoracic duct ligation" nil (:question :name "other surgery: thoracic duct ligation: date") "<small><i>mm/dd/yy</i></small>")
               ((:question :name "other surgery: nephrectomy" :data-type :boolean :view-type :checkbox )
                "Nephrectomy" nil (:question :name "other surgery: nephrectomy: date") "<small><i>mm/dd/yy</i></small>")
               ((:question :name "other surgery: hysterectomy" :data-type :boolean :view-type :checkbox )
                "Hysterectomy" nil (:question :name "other surgery: hysterectomy: date") "<small><i>mm/dd/yy</i></small>")
               ((:question :name "other surgery: other" :data-type :boolean :view-type :checkbox )
                "Other" (:question :name "other surgery: other: specify" :data-type :string)
                (:question :name "other surgery: other: date") "<small><i>mm/dd/yy</i></small>")))
             (other/transplant/question
              (apply #'make-question "Transplant/Transplant Evaluation"
                     :parent *group*
                     (choices-options-yes-no)))
             (other/transplant/subgroup
              (let* ((subgroup 
                      (make-instance 'survey-group 
                                     :name "transplant eval subgroup"
                                     :questions other/transplant/question))
                     (subquestion
                      (apply #'make-question "Please specify:"
                             :parent other/transplant/question
                             (radio-options
                              (choices-breaks-alist
                               '(("The patient has not required transplant evaluation" . "not required")
                                 ("The patient was evaluated, but has not had a transplant" . "evaluated")
                                 ("The patient had a transplant" . "transplant"))))))
                     (question2
                      (apply #'make-question "transplant lungs" :prompt ""
                             (radio-options '(("One lung" . 1) ("Both lungs" . 2)))))
                     (question3 (make-question "Transplant date" :data-type :date :data-subtype :date-month-year))
                     (subgroup2
                      (make-survey-sub-group-named *group* "transplant/transplant evaluation question subgroup"
                                                   :order (list question2 question3))))
                ;; Group rules
                (setf (group-questions subgroup) (list subquestion))
                (add-rule subgroup subquestion "transplant" subgroup2 ':inline)
                ;; Returns
                subgroup)))

              
        ;; Group rules
        (add-rule *group* hormone-therapy/question "Yes" hormone-therapy/table ':inline)
        (add-rule *group* bronch/meds/question "Yes" bronch/meds/table ':inline)
        (add-rule *group* other/meds/question "Yes" other/med/table ':inline)
        (add-rule *group* other/surgery/question "Yes" other/surgery/table ':inline)
        (add-rule *group* other/transplant/question "Yes" other/transplant/subgroup ':inline)
        ;; Questions
        (setf (group-questions *group*)
              (list no-treatment hormone-therapy/question bronch/meds/question other/meds/question
                    pneumothorax/pleural-effusion/question other/surgery/question
                    other/transplant/question)))

      ;;
      ;; Home oxygen use
      ;;
      (let* ((*group*
              (make-survey-group-named survey-clinician #!"Home oxygen use"))
             (q20
              (let* ((subgroup
                      (make-survey-sub-group-named *group* "q20 subgroup"))
                     (question
                      (apply #'make-question "Does/did the patient use oxygen at home"
                             :number 20.
                             :prompt-format prompt-format-numbered-question
                             :parent *group*
                             (choices-options-yes-no-unknown)))
                     (q/how
                      (let* ((question
                              (apply #'make-question "If yes the patient uses oxygen at home"
                                     :prompt "If yes:"
                                     (radio-options
                                      (choices-mirror-alist
                                       '("Continuous" "With activities" "At night" "Other")))))
                             (q/other (make-question "Other oxygen use" :prompt "Other:"))
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
                                     :data-type :date :data-subtype :date-month-year)))
                ;; Questions
                (setf (group-questions subgroup) (list q/how q/how-much q/how-long))
                ;; Group rules
                (add-rule *group* question "Yes" subgroup ':inline)
                ;; Returns
                question)))
        ;; Questions
        (setf (group-questions *group*) (list q20)))

      ;;
      ;; Vital status
      ;;
      (let* ((*group*
              (make-survey-group-named survey-clinician #!"Vital status"))
             (q21
              (apply #'make-question "What is the patient's vital status" :number 21.
                     :prompt-format prompt-format-numbered-question
                     :parent *group*
                     (radio-options
                      (choices-mirror-alist
                       '("Living" "Deceased")))))
             (q21a (make-question "Date of last confirmation"
                                  :prompt-format prompt-format-colon
                                  :data-type :date :data-subtype :date-month-year))
             (subgroup21-living (make-survey-sub-group-named *group* nil :order (list q21a)))
             (q21b (make-question "Date of death" :data-type :date :data-subtype :date-full))
             (q21c
              (apply #'make-question "What was the cause of death"
                     :prompt-format prompt-format-question
                     (radio-options
                      (choices-breaks-alist
                       '("Respiratory failure" "Infection" "Pulmonary thromboembolism" "Cancer" "Other" "Unknown")))))
             (subgroup21-dead (make-survey-sub-group-named *group* nil :order (list q21b q21c)))
             (q21d (make-question "Please indicate type of infection if known" :data-type :string))
             (subgroup21-infection (make-survey-sub-group-named *group* nil :order (list q21d)))
             (q21e (make-question "Please indicate type of cancer if known" :data-type :string))
             (subgroup21-cancer (make-survey-sub-group-named *group* nil :order (list q21e)))
             (q21f (make-question "Please indicate cause of death if known" :data-type :string))
             (subgroup21-other (make-survey-sub-group-named *group* nil :order (list q21f))))
        ;; Group rules
        (setf (group-questions *group*) (list q21))
        (add-rule *group* q21 "Living" subgroup21-living ':inline)
        (add-rule *group* q21 "Deceased" subgroup21-dead ':inline)
        (add-rule subgroup21-dead q21c "Infection" subgroup21-infection :inline)
        (add-rule subgroup21-dead q21c "Cancer" subgroup21-cancer :inline)
        (add-rule subgroup21-dead q21c "Other" subgroup21-other :inline))

      ;;
      ;; Results from other surveys
      ;;
      (let* ((*group*
              (make-survey-group-named survey-clinician #!"SGRQ & Six Minute Walk Distance"))
             (q22
              (let ((question
                     (apply #'make-question "Does the patient have <B>six minute walk distance</B> (6MWD) results"
                            :number 22.
                            :prompt-format prompt-format-numbered-question
                            :parent *group*
                            (choices-options-yes-no)))
                    (subgroup
                     (make-survey-sub-group-named *group* "q22 if yes"
                                                  :advice "Please fill out the 6MWD diary under the Collect tab")))
                ;; Group rules
                (add-rule *group* question "Yes" subgroup ':inline)
                ;; Returns
                question))
             (q23
              (let ((question
                     (apply #'make-question "Has the patient taken the <B>St. George's Respiratory Questionnaire (SGRQ)</B>"
                            :number 23.
                            :prompt-format prompt-format-numbered-question
                            :parent *group*
                            (choices-options-yes-no)))
                    (subgroup
                     (make-survey-sub-group-named *group* "q23 if yes"
                                                  :advice "Please fill out the SGRQ diary under the Collect tab")))
                ;; Group rules
                (add-rule *group* question "Yes" subgroup ':inline)
                ;; Returns
                question)))
        ;; Group questions
        (setf (group-questions *group*) (list q22 q23)))

      ;;
      ;; Survey - treatment/surgery
      ;;
      (flet ((make-treatment-questions (name group)
               (let* ((question
                       (apply #'make-question name
                              :prompt-format prompt-format-colon
                              :parent group
                              (multi-choices-options
                               (choices-mirror-alist
                                '("Chest tube placement/pleural drainage"
                                  "Open chest surgery"
                                  "Thoracoscopic/minimally invasive chest surgery"
                                  "Pleurodesis"
                                  "Other"
                                  "None")))))
                      (subq1 (make-question "Type of open chest surgery"
                                            :prompt-format prompt-format-colon))
                      (subg1
                       (let ((subgroup
                              (make-survey-sub-group-named group "open chest surgery subgroup" :order (list subq1))))
                         (add-rule group question "Open chest surgery" subgroup ':inline)
                         subgroup))
                      (subq2 (make-question "Type of thoracoscopic/minimally invasive chest surgery"
                                            :prompt-format prompt-format-colon))
                      (subg2
                       (let ((subgroup
                              (make-survey-sub-group-named group "thoracoscopic/minimally invasive chest surgery subgroup" :order (list subq2))))
                         (add-rule group question "Thoracoscopic/minimally invasive chest surgery" subgroup ':inline)
                         subgroup))
                      (subg3
                       (let* ((q1 (apply #'make-question "Pleurodesis side"
                                         :prompt-format prompt-format-colon
                                         (radio-options (choices-mirror-alist '("Left" "Right" "Both")))))
                              (subgroup
                               (make-survey-sub-group-named group "pleurodesis subgroup"))
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
                         (add-rule group question "Pleurodesis" subgroup ':inline)
                         (setf (group-questions subgroup) (list q1 q2))
                         ;; Returns
                         subgroup))
                      (subq4 (make-question "Other treatment - please specify"
                                            :prompt-format prompt-format-colon))
                      (subg4
                       (let ((subgroup
                              (make-survey-sub-group-named group "other treatment - specify" :order (list subq4))))
                         (add-rule group question "Other" subgroup ':inline)
                         subgroup)))
                 (declare (ignore subg1 subg2 subg3 subg4))
                 ;; Returns
                 question)))
        (let* ((*group*
                (make-survey-group-named survey-treatment (name survey-treatment)))
               (q1 (make-question "Date of treatment/surgery" :prompt-format prompt-format-colon
                                  :data-type :date :data-subtype :date-full :parent *group*))
               (q2 (apply #'make-question "Treatment of"
                          :parent *group*
                          :prompt-format prompt-format-colon
                          (radio-options
                           (choices-breaks-alist
                            '("Pneumothorax"
                              "Pleural effusion"
                              "Pneumothorax and pleural effusion")))))
               (q3 (make-treatment-questions "Treatment" *group*)))
          ;; Questions
          (setf (group-questions *group*) (list q1 q2 q3))
          ;; Survey properties
          (setf (diary-question survey-treatment) q1)))

      ;;
      ;; Survey - PFT diary
      ;;
      (let* ((group1
              (make-survey-group-named survey-pft "Test conditions"))
             (q1
              (make-question "Date of Pulmonary Function Test"
                             :prompt-format prompt-format-colon
                             :data-type :date :data-subtype :date-full
                             :parent group1))
             (q2
              (apply #'make-question "Please check if the patient had any of the following at the time of this PFT result"
                     :prompt-format prompt-format-colon
                     :parent group1
                     (multi-choices-options
                      (choices-mirror-alist
                       '("Pneumothorax" "Pleural effusion" "Pneumonia" "Chest surgery within the previous 6 months" "Unknown")))))
             ;; Group 2 is a table of questions
             (group2
              (let ((table
                     (make-pft-question-table :before t
                                              :owner owner
                                              :advice "These results are <B>PRE-BRONCHODILATOR</B> results:")))
                (setf (group-name table) "Pre-bronchodilator results")
                (setf (survey-groups survey-pft) (append (survey-groups survey-pft) (list table)))
                ;; Returns
                table))
             ;; Second set, repeat of questions post-bronchodilator
             (group3
              (make-survey-group-named survey-pft "Post-bronchodilator results"))
             (q4
              (apply #'make-question "Were tests performed <B>POST-BRONCHODILATOR</B>"
                     :prompt-format "~A (examples: albuterol or ipratroprium inhaler/nebulizer)?"
                     :parent group3
                     (choices-options-yes-no)))
             (q4subgroup (make-pft-question-table :before nil :owner owner)))
        (declare (ignore group2))
        ;; Group rules
        (setf (group-questions group1) (list q1 q2))
        (setf (group-questions group3) (append (list q4)))
        (add-rule group3 q4 "Yes" q4subgroup ':successor)
        ;; Survey properties
        (setf (diary-question survey-pft) q1))

      ;;
      ;; Survey - 6MWD diary
      ;;
      (let* ((*group*
              (make-survey-group-named survey-6mwd (name survey-6mwd)))
             (q1 (make-question "Date"
                                :prompt-format prompt-format-colon
                                :parent *group*
                                :data-type :date :data-subtype :date-full))
             (q2 (make-question "Result" :prompt-format prompt-format-colon
                                :parent *group*
                                ;; Using the measurement data type facility here may be confusing for users
                                ;; because the default units are feet and cm
                                ;; :data-type :measurement :data-subtype :length
                                :data-type :string))
             (q3 (apply #'make-question "Result units"
                        :prompt-format prompt-format-colon
                        :parent *group*
                        (radio-options
                         (choices-mirror-alist '("feet" "meters"))))))
        ;; Group questions
        (setf (group-questions *group*) (list q1 q2 q3))
        ;; Survey diary question
        (setf (diary-question survey-6mwd) q1))

      ;;
      ;; Survey - SGRQ diary
      ;;
      (let* ((*group*
              (make-survey-group-named survey-sgrq (name survey-sgrq)))
             (q1 (make-question "Date"
                                :prompt-format prompt-format-colon
                                :parent *group*
                                :data-type :date :data-subtype :date-full))
             (q2 (make-question "Total Score"
                                :prompt-format prompt-format-colon
                                :parent *group*
                                :data-type :number))
             (q3 (make-question "Symptoms Score"
                                :prompt-format prompt-format-colon
                                :parent *group*
                                :data-type :number))
             (q4 (make-question "Activity Score"
                                :prompt-format prompt-format-colon
                                :parent *group*
                                :data-type :number))
             (q5 (make-question "Impacts Score"
                                :prompt-format prompt-format-colon
                                :parent *group*
                                :data-type :number)))
        (setf (group-questions *group*) (list q1 q2 q3 q4 q5))
        ;; Survey diary question
        (setf (diary-question survey-sgrq) q1))

      ;; Returns
      `( (,survey-intro :DOFIRST)
         (,survey-clinician :DOFIRST)
         (,survey-pft :REQUIRED)
         (,survey-treatment :OPTIONAL)
         (,survey-6mwd :OPTIONAL)
         (,survey-sgrq :OPTIONAL))
      )))

(defun create-lamsight-qol/pft-survey (&key (owner (current-user)))
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
                   :description #!"Enter demographics, symptoms, method of diagnosis, and medical treatment history" ;??
                   survey-args)))

      ;;
      ;; Main patient survey - LAMsight QOL/PFT Patient Questionnaire
      ;;

      ;;
      ;; Group 1 - Patient header page
      ;;
      (make-survey-group-named survey-patient #!"Introduction"
                               :advice "Thank you for agreeing to take part in this research study! We hope to use this information to answer questions that are important to you and the LAM community.
<P>We expect that the following questionnaires will take you about 20-30 minutes. You do not need to complete all of the questions in one setting, however we do ask that you complete two of the surveys (the SF-36 and SGRQ) within one week of each other. You may skip any question that you are uncomfortable answering. If any of the questions are unclear, please feel free to contact
<A HREF=\"mailto:LAMResearchStudy@partners.com\">LAMResearchStudy@partners.org</A> with comments or questions.
<P><B>You will need copies of your pulmonary function test (PFT) reports to complete this study.</B>
You may need to contact your health care provider to obtain these reports. 
In the last part of this study, we will ask you to enter your <B>most recent</B> PFT results on an online collection form.
Finally, we will ask you to submit to us all of your previous pulmonary function test (PFT) results, including your most recent report. Your pulmonary function results can be entered and submitted at any time.")

      ;;
      ;; Group 2 - Patient info
      ;;
      (let* ((*group* (make-survey-group-named survey-patient #!"Demographics"))
             (q1 (make-question "Name"
                                :number 1. :prompt "Name (First, Last)" :prompt-format prompt-format-numbered-colon
                                :parent *group*
                                :hipaa-id-p t))
             (q2 (make-question "Date of birth"
                                :number 2.
                                :prompt-format prompt-format-numbered-colon
                                :parent *group*
                                :data-type :date :data-subtype :date-full))
             (q3 (make-question "What country do you live in"
                                :number 3.
                                :prompt-format prompt-format-numbered-question
                                :parent *group*))
             (q4 (make-question "How many total years of schooling or education have you completed"
                                :number 4. 
                                :prompt-format prompt-format-numbered-question :data-type :number
                                :parent *group*))
             (q5
              (let* ((question
                      (apply #'make-question "Are you completing this survey in a language other than your primary language"
                             :number 5. 
                             :prompt-format prompt-format-numbered-question
                             :parent *group*
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
                                :parent *group*
                                :data-type :measurement
                                :data-subtype :length))
             (q7 (make-question "What is your weight"
                                :number 7.
                                :prompt-format prompt-format-numbered-question
                                :parent *group*
                                :data-type :measurement
                                :data-subtype :weight))
             (q8 (apply #'make-question "What is your ethnicity"
                        :number 8.
                        :prompt-format prompt-format-numbered-question
                        :parent *group*
                        (radio-options
                         (choices-breaks-alist '(("Hispanic or Latino" . t) ("Not Hispanic or Latino" . nil))))))
             (q9
              (let* ((question
                      (apply #'make-question "What is your race"
                             :number 9.
                             :prompt-format prompt-format-numbered-question
                             :parent *group*
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
                             :prompt-format prompt-format-numbered-question
                             :parent *group*
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
        
      ;; Group 3 - Symptoms & diagnosis
      ;;
      (let* ((*group*
              (make-survey-group-named survey-patient #!"Symptoms & diagnosis"))
             (q11
              (apply #'make-question "Diagnosis type"
                     :number 11.
                     :prompt "I was diagnosed with"                     
                     :prompt-format prompt-format-numbered-colon
                     :parent *group*
                     (radio-options
                      '(("Sporadic LAM" . "LAM")
                        ("Tuberous Sclerosis Complex with LAM (TSC-LAM)" . "TSC-LAM")
                        ("I don't know" . "Unknown")))))
             ;; Rule: if q11 answer is TSC-LAM...
             (q12
              (apply #'make-question  "If you have TSC-LAM, what signs of TSC do you have"
                     :number 12.
                     :prompt-format prompt-format-numbered-question
                     :parent *group*
                     (multi-choices-options
                      (choices-mirror-alist
                       '("Skin" "Brain" "Kidney" "Eye" "Other")))))
             ;; Rule: if previous answer included Other...
             (q12o
              (make-question "Other TSC-LAM symptoms" :prompt-format prompt-format-colon
                                                      :data-type :string :view-type :text-field
                                                      :parent *group*))

             (q13
              (make-question "Date of diagnosis of LAM or TSC-LAM"
                             :number 13.
                             :prompt-format prompt-format-numbered-colon
                             :data-type :date :data-subtype :date-month-year
                             :parent *group*))
             (q14
              (make-question "When did you first have LAM related symptoms"
                             :number 14.
                             :prompt-format prompt-format-numbered-question
                             :data-type :date :data-subtype :date-month-year
                             :parent *group*))
             (q15
              (let* ((question
                      (apply #'make-question "What were your main symptoms that led to your diagnosis of LAM"
                             :number 15.
                             :prompt-format prompt-format-numbered-question
                             :parent *group*
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
                             :parent *group*
                             (multi-choices-options
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
              (make-survey-group-named survey-patient #!"Smoking history and complications of LAM"))
             (q17
              (let* ((question
                      (apply #'make-question "Do you smoke or did you smoke in the past"
                             :number 17.
                             :prompt-format prompt-format-numbered-question
                             :parent *group*
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
                       ("I quit" (:question :name "quit months ago") "months ago")
                       ("" (:question :name "quit years ago") "years ago"))))
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
                             :parent *group*
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
                     :parent *group*
                     (radio-options
                      (choices-mirror-alist '("Yes" "No" "I don't know")))))
             (q20
              (apply #'make-question "Have you ever had abdominal fluid or ascites"
                     :number 20.
                     :prompt-format prompt-format-numbered-question
                     :parent *group*
                     (radio-options
                      (choices-mirror-alist '("Yes" "No" "I don't know"))))))
        ;; Questions
        (setf (group-questions *group*) (list q17 q18 q19 q20)))

      ;;
      ;; Group 5 - Treatment, part 1
      ;; 
      (let* ((*group*
              (make-survey-group-named survey-patient #!"Treatment, part 1"))
             (q21
              (let* ((question
                      (apply #'make-question "Have you ever had a pleurodesis"
                             :number 21.
                             :prompt-format (concatenate 'string
                                                         prompt-format-numbered-question
                                                         " This is a procedure to make your lung adhere to your chest wall to treat a pleural effusion or pneumothorax.")
                             :parent *group*
                             (radio-options
                              (choices-mirror-alist '("Yes" "No" "I don't know")))))
                     (q/how-many
                      (make-question "If yes how many pleurodeses"
                                     :prompt-format prompt-format-question
                                     :data-type :number))
                     (q/which-lungs
                      (apply #'make-question "pleurodesis which lungs"
                             :prompt-format ""
                             :parent *group*
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
                             :parent *group*
                             (radio-options
                              (choices-mirror-alist '("Yes" "No" "I don't know")))))
                     (q/treatments
                      (apply #'make-question "Other treatments"
                             :prompt-format ""
                             (multi-choices-options
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
                             :parent *group*
                             (choices-options-yes-no)))
                     (q/which-lungs
                      (apply #'make-question "transplant which lungs" :prompt-format ""
                             (radio-options '(("One lung" . 1) ("Both lungs" . 2)))))
                     (q/date (make-question "Transplant date" :data-type :date :data-subtype :date-month-year))
                     (subgroup
                      (make-survey-sub-group-named *group* "transplant question subgroup"
                                                   :order (list q/which-lungs q/date))))
                ;; Group rules
                (add-rule *group* question "Yes" subgroup ':inline)
                ;; Returns
                question)))
        ;; Questions
        (setf (group-questions *group*) (list q21 q22 q23)))

      ;;
      ;; Group 6 - misc
      ;;
      (let* ((*group*
              (make-survey-group-named survey-patient #!"Miscellaneous"))
             (q24
              (let* ((subgroup
                      (make-survey-sub-group-named *group* "q24 subgroup"))
                     (question
                      (apply #'make-question "Do you use oxygen at home"
                             :number 24.
                             :prompt-format prompt-format-numbered-question
                             :parent *group*
                             (choices-options-yes-no)))
                     (q/how
                      (let* ((question
                              (apply #'make-question "If yes you use oxygen at home"
                                     :prompt "If yes:"
                                     (radio-options
                                      (choices-mirror-alist
                                       '("Continuous" "With activities" "At night" "Other")))))
                             (q/other (make-question "Other oxygen use" :prompt "Other:"))
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
                      (make-question "When did you begin using oxygen at home"
                                     :prompt-format prompt-format-question
                                     :data-type :date :data-subtype :date-month-year)))
                ;; Questions
                (setf (group-questions subgroup) (list q/how q/how-much q/how-long))
                ;; Group rules
                (add-rule *group* question "Yes" subgroup ':inline)
                ;; Returns
                question))
             (q25
              (apply #'make-question "Do you have kidney disease (for example, angiomyolipomas (AMLs))"
                     :number 25.
                     :prompt-format prompt-format-numbered-question
                     :parent *group*
                     (radio-options
                      (choices-mirror-alist '("Yes" "No" "I don't know")))))
             (q26
              (apply #'make-question "What <B>best</B> describes your level of breathlessness during activity"
                     :prompt-format prompt-format-numbered-question
                     :number 26.
                     :parent *group*
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
      ;; Group 7 - estrogen exposure
      ;;
      (let* ((*group*
              (make-survey-group-named survey-patient #!"Estrogen exposure history"))
             (q27
              (let* ((question
                      (apply #'make-question "Did you take hormonal contraception (birth control pills) before you were diagnosed with LAM"
                             :number 27.
                             :prompt-format prompt-format-numbered-question
                             :parent *group*
                             (radio-options
                              (choices-mirror-alist '("Yes" "No" "I don't know")))))
                     (q/type
                      (make-question "Please specify type"
                                     :prompt-format prompt-format-colon
                                     :data-type :string))
                     (q/date
                      (make-question "Dates of use"
                                     :prompt-format prompt-format-colon
                                     :data-type :date-range :data-subtype :date-month-year
                                     :help "Use 'present' for end date when appropriate"))
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
                             :parent *group*
                             (radio-options
                              (choices-mirror-alist '("Yes" "No" "I don't know")))))
                     (table28
                      (make-survey-group-table (:name "question27 if yes" :advice "If yes: please complete, if unknown please leave blank:" :default-question-args (:data-type :number))
                                       ( nil "Number <B>before</B> your diagnosis of LAM"
                                             "Unknown"
                                             "Number <B>during</B> or <B>after</b> your diagnosis of LAM"
                                             "Unknown")
                                       ( "Number of pregnancies" 
                                         (:question) 
                                         (:question :name "number of pregnancies before unknown"
                                                    :data-type :boolean
                                                    :view-type :checkbox)
                                         (:question)
                                         (:question :name "number of pregnancies during/after unknown"
                                                    :data-type :boolean
                                                    :view-type :checkbox))
                                       ( "Number of births" 
                                         (:question) 
                                         (:question :name "number of births before unknown"
                                                    :data-type :boolean
                                                    :view-type :checkbox)
                                         (:question)
                                         (:question :name "number of births during/after unknown"
                                                    :data-type :boolean
                                                    :view-type :checkbox)))))
                ;; Group rules
                (add-rule *group* question "Yes" table28 ':successor)
                ;; Returns
                question))
             (q29
              (let* ((question
                      (apply #'make-question "Have you gone through menopause"
                             :number 29.
                             :prompt-format prompt-format-numbered-question
                             :parent *group*
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
      ;; Group 8 - treatment, part 2
      ;;
      (flet ((make-table-current-past-use (prefix)
               (let ((table
                      (make-survey-group-table
                       (:name "current or past use table"
                        :default-question-args (:data-type :date :data-subtype :date-month-year))
                       (nil nil nil nil nil)
                       ((:question :name "current use answer" :data-type :boolean :view-type :checkbox)
                        "Current use."
                        "When did you start?" (:question :name "Start date" :data-type :date :data-subtype :date-month-year) 
                        nil nil)
                       ((:question :name "past use answer" :data-type :boolean :view-type :checkbox)
                        "Past use."
                        "Start date:" (:question :name "Start date" :data-type :date :data-subtype :date-month-year)
                        "End date:" (:question :name "End date" :data-type :date :data-subtype :date-month-year)))))
                 ;; Modify all question names based on before/after flag
                 (dolist (question (group-questions table))
                   (setf (question-name question)
                         (format nil "~A - ~A" prefix (question-name question))))
                 ;; Returns
                 table)))
        (let* ((*group*
                (make-survey-group-named survey-patient #!"Treatment, part 2"
                                         :advice "<SUP>30</SUP> What LAM related <B>treatments</B> have you had or are you currently using? <SMALL>Please check all that apply</SMALL>"))
               (bronch/meds/question
                (let* ((question
                        (apply #'make-question "Bronchodilator therapy"
                               :prompt "Bronchodilators (Examples: albuterol, ipratroprium inhalers or nebulizers)"
                               :parent *group*
                               (choices-options-yes-no)))
                       (table (make-table-current-past-use "Bronchodilator")))
                  ;; Group rules
                  (add-rule *group* question "Yes" table ':inline)
                  ;; Returns
                  question))
               (steroids/meds/question
                (let* ((question
                        (apply #'make-question "Inhaled steroids therapy"
                               :prompt "Inhaled steroids"
                               :parent *group*
                               (choices-options-yes-no)))
                       (table (make-table-current-past-use "Inhaled steroids")))
                  ;; Group rules
                  (add-rule *group* question "Yes" table ':inline)
                  ;; Returns
                  question))
               (hormone-therapy/question
                (let* ((question
                        (apply #'make-question "Hormone therapy"
                               :prompt "Anti-estrogen (hormonal) medical therapy (Examples: progesterone pills or injection, GnRH, Tamoxifen)"
                               :parent *group*
                               (choices-options-yes-no)))
                       (table (make-table-current-past-use "Hormones")))
                  ;; Group rules
                  (add-rule *group* question "Yes" table ':inline)
                  ;; Returns
                  question))
               (sirolimus/rapamune/question
                (let* ((question
                        (apply #'make-question "Sirolimus/Rapamune therapy"
                               :prompt "Sirolimus/Rapamune"
                               :parent *group*
                               (choices-options-yes-no)))
                       (table (make-table-current-past-use "Sirolimus/Rapamune")))
                  ;; Group rules
                  (add-rule *group* question "Yes" table ':inline)
                  ;; Returns
                  question))
               (ovaries/question
                (let* ((question
                        (apply #'make-question "I have had my ovaries removed"
                               :parent *group*
                               (choices-options-yes-no)))
                       (q/date (make-question "Date ovaries removed"
                                              :prompt "Date:"
                                              :data-type :date :data-subtype :date-month-year))
                       (q/therapy
                        (apply #'make-question "If yes do/did you use hormone replacement therapy"
                               :prompt-format prompt-format-question
                               (radio-options
                                (choices-mirror-alist '("Yes" "No" "I don't know")))))
                       (subgroup (make-survey-sub-group-named *group* "ovaries removed subgroup" :order (list q/date q/therapy))))
                  ;; Group rules
                  (add-rule *group* question "Yes" subgroup ':inline)
                  ;; Returns
                  question))
               (other/question
                (let* ((question
                        (apply #'make-question "Other therapy"
                               :prompt "Other:"
                               :parent *group*
                               (choices-options-yes-no)))
                       (q/other (make-question "Other therapy - please specify"
                                               :prompt-format prompt-format-colon))
                       (subgroup (make-survey-sub-group-named *group* "other therapy subgroup" :order (list q/other))))
                  ;; Group rules
                  (add-rule *group* question "Yes" subgroup ':inline)
                  ;; Returns
                  question))
               (none/question
                (apply #'make-question "I have not undergone treatment"
                       :parent *group*
                       (choices-options-yes-no))))
          ;; Questions
          (setf (group-questions *group*)
                (list bronch/meds/question steroids/meds/question
                      hormone-therapy/question sirolimus/rapamune/question ovaries/question
                      other/question none/question))))

      ;;
      ;; Groups 9, 10, 11 - PFT
      (let* ((*group*
              (make-survey-group-named survey-patient #!"Pulmonary Function Tests (PFT), part 1"
                                       :advice "You will need a copy of your <B>most recent</B> pulmonary function test report to complete this part of the study. You may need to contact your health care provider to get a copy of this report."))
             (q1
              (make-question "Date of your most recent Pulmonary Function Test"
                             :prompt "Date of your <B>most recent</B> Pulmonary Function Test:"
                             :parent *group*
                             :data-type :date :data-subtype :date-month-year))
             (q2
              (make-question "Check here if you have not had pulmonary function tests performed in the last 5 years."
                             :parent *group*
                             :data-type :boolean :view-type :checkbox))
             (q3
              (apply #'make-question "Did you have any of the following at the time of this PFT result"
                       :prompt-format prompt-format-question
                       :parent *group*
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
                (setf (group-name table) #!"PFTs, part 2")
                (setf (survey-groups survey-patient) (append (survey-groups survey-patient) (list table)))
                ;; Returns
                table))
             ;; Second set, repeat of questions post-bronchodilator
             (q4
              (apply #'make-question "Did you have tests performed POST-BRONCHODILATOR"
                     :prompt "Did you have tests performed <B>POST-BRONCHODILATOR</B> (examples: albuterol or ipratroprium inhaler/nebulizer)?"
                     (radio-options
                      (choices-mirror-alist '("Yes" "No" "I don't know")))))
             (group3
              (make-survey-group-named survey-patient #!"PFTs, part 3"
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
             (make-survey-group-named survey-patient #!"PFT report submission"
                                      :advice "<P>Thank you for completing this form!
<P>Please submit a copy of <B>all</B> of your pulmonary function test reports including your most recent PFT report (the same report that you used to enter information on this site for this study).
<P>You may do this in any of the following ways:
<P>Email: <A HREF=\"mailto:LAMResearchStudy@partners.org\">LAMResearchStudy@partners.org</A>
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
      survey-patient)))

(defun create-lamsight-qol/pft-surveys (&key (owner (current-user t)) study-name)
  (let ((survey-patient (create-lamsight-qol/pft-survey :owner owner))
        (survey-sf36 (create-qualitymetric-sf36-survey :owner owner))
        (surveys-sgrq (create-sgrq-surveys :owner owner :study-name study-name)))
    ;; Returns
    `((,survey-patient :DOFIRST)
      ,@(loop for survey in surveys-sgrq
           collect `(,survey :OPTIONAL))
      (,survey-sf36 :OPTIONAL ,(namestring (qualitymetric-start-page-pathname))))))

;;; Create studies

(defun create-ilr-arr/pft-study (&key (owner (current-user)) (study-name +study-name-clinician+) surveys)
  (let ((survey-specs (or surveys (create-ilr-arr/pft-surveys :owner owner)))
        (study
         (make-instance 'study :name study-name
                               :description #!"Retrospective medical record review to examine the rate of pulmonary function decline for LAM patients diagnosed at age 25 and younger relative to those diagnosed over age 55"
                               :published t :owner owner :priority 1 :origin "researcher")))
    (collect-study-surveys-and-rules study survey-specs)
    ;; Returns
    study))

(defun create-lamsight-qol/pft-study (&key (owner (current-user)) (study-name +study-name-patient+))
  (let ((survey-rule-alist (create-lamsight-qol/pft-surveys :owner owner :study-name study-name))
        (study
         (make-instance 'study :name study-name
                               :description #!"This study examines the relationship between pulmonary function and quality of life in patients with LAM"
                               :requires-consent-p t
                               :patient-consent-forms
                               '(("lam-qol-study-consent-form" :signature :date :time)
                                 ("lam-qol-study-ilr-data-use-form" :yes-no-initials))
                               :articles-page-name "lam-qol-study-articles"
                               :study-complete-message #!"Thank you for completing this study! Please remember to submit your pulmonary function test results."
                               :published t :owner owner :priority 1 :origin "researcher")))
    (loop for spec in survey-rule-alist
         with surveys 
         with rules
         do (let ((survey (first spec))
                  (rule-type (second spec))
                  (url (third spec)))
              (push survey surveys)
              ;; Here we could use ADD-SURVEY-RULE but this is faster
              (push (make-survey-rule :survey survey :type rule-type :url url) rules))
         finally
         (setf (surveys study) (reverse surveys) (survey-rules study) rules))
    ;; Returns
    study))

;;; Data analysis and reporting

(defun get-ilr-arr/pft-questions (survey)
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

(defun create-ilr-arr/pft-data (&key (count 100.) (center "lamhtest"))
 (let ((survey +survey-name-clinician+))
  (multiple-value-bind (questions question-count) (get-ilr-arr/pft-questions survey)

    (unless (plusp question-count)
      (error "Survey questions not found for ~S" survey))

    ;; Create or initialize center
    (with-transaction ()
      ;; Coerce center
      (when (stringp center)
        (setq center
              (or (get-center center t)
                  (make-center center "ILR ARR/PFT test center"))))
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
                           (prog1 "TSC-LAM"
                             ;; Symptoms
                             (let (answer)
                               (block gather-symptoms
                                 (dolist (symptom (mapcar #'cdr (question-choices q5)))
                                   (when (= (random 3 q5-symptoms-rs) 1)
                                     (when (member symptom '("None" "Unknown") :test #'string-equal)
                                       (setq answer (list symptom))
                                       (return-from gather-symptoms))
                                     (push symptom answer))))
                               (and answer (add-answer q5 patient answer)))
                             ;; Organs affected
                             (let (answer)
                               (block gather-symptoms
                                 (dolist (symptom (mapcar #'cdr (question-choices q6)))
                                   (when (= (random 3 q6-symptoms-rs) 1)
                                     (when (member symptom '("None" "Unknown/not screened") :test #'string-equal)
                                       (setq answer (list symptom))
                                       (return-from gather-symptoms))
                                     (push symptom answer))))
                               (and answer (add-answer q6 patient answer)))))
                          (t "LAM"))))
          (let (answer)
            (block gather-symptoms
              (dolist (symptom (mapcar #'cdr (question-choices q7)))
                (when (= (random 5. q7-diagnosis-method-rs) 1)
                  (when (member symptom '("Unknown") :test #'string-equal)
                    (setq answer (list symptom))
                    (return-from gather-symptoms))
                  (push symptom answer))))
            (and answer (add-answer q7 patient answer)))
          (let (answer)
            (block gather-symptoms
              (dolist (symptom (mapcar #'cdr (question-choices q9)))
                (when (= (random 5. q9-origin-rs) 1)
                  (when (member symptom '("Unknown") :test #'string-equal)
                    (setq answer (list symptom))
                    (return-from gather-symptoms))
                  (push symptom answer))))
            (and answer (add-answer q9 patient answer)))
          (add-answer q20 patient (= (random 7. q20-rs) 1.))
                      
          ))))
    ;; Done
    )))

(defun create-ilr-arr/pft-report (&key survey (center "lamhtest")
                                  (questions '(3 4 5 6 7 9 10 15 20))
                                  (stream *standard-output*))
  (assert (not (null survey)))
  (if (stringp center)
      (setq center (get-center center)))
  (let* ((qarray (get-ilr-arr/pft-questions survey))
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
    

(defun get-lamsight-qol/pft-questions (survey)
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

(defun create-lamsight-qol/pft-data (&key (count 15.))
 (let ((survey +survey-name-patient+)
       (center (get-patient-home-center)))
  (multiple-value-bind (questions question-count) (get-ilr-arr/pft-questions survey)

    (unless (plusp question-count)
      (error "Survey questions not found for ~S" survey))

    ;; Create test data
    (with-transaction ()
      (let (
            ;; Random states for questions that define strata for our "sample"
            (q3-country-rs (make-random-state t))
            (q8-ethnicity-rs (make-random-state t))
            (q9-race-rs (make-random-state t))
            (q11-diagnosis-rs (make-random-state t))
            (q17-smoker-rs (make-random-state t))
            )
        
      (dotimes (n count)
        (let* ((patient (make-patient (generate-patient-id :center center) center))
               (user (user-add (id patient) (id patient)))
               (q3 (aref questions 3.)) ;country of origin
               (q8 (aref questions 8.)) ;ethnicity
               (q9 (aref questions 9.)) ;race
               (q11 (aref questions 11.)) ;diagnosis
               (q17 (aref questions 17.)) ;smoker?
               )
          ;; Fix up user and patient
          (setf (external-id patient) (symbol-name (gensym)))
          (setf (user patient) user)
          (setf (get-preference :lam-patient-p user) t)

          ;; Create test survey answers

          (add-answer q3 patient (nth (random 5. q3-country-rs) '("United States" "France" "Spain" "United Kingdom" "Brazil")))
          (add-answer q8 patient (> (random 6. q8-ethnicity-rs) 2.))
          (add-answer q9 patient
                      (let ((val (random 10. q9-race-rs)))
                        (cond
                          ((< val 7.) "White")
                          ((< val 9.) "Black or African American")
                          (t "Asian"))))
          (add-answer q11 patient (if (> (random 6. q11-diagnosis-rs) 2.) "LAM" "TSC-LAM"))
          (add-answer q17 patient
                      (let ((val (random 6. q17-smoker-rs)))
                        (cond
                          ((< val 3) "I never smoked")
                          ((< val 5) "Yes I smoke")
                          (t "I quit"))))
          ))))
    ;; Done
    )))

(defun create-lamsight-qol/pft-report (&key survey 
                                       (questions '(3 8 9 11 17))
                                       (stream *standard-output*))
  (let* ((qarray (get-ilr-arr/pft-questions survey))
         (center (get-patient-home-center))
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
    
