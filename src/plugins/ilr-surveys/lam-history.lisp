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
    (let* ((*question-prompt-suffix* ":")
           (survey
            (make-survey-named +survey-name-lam-history+
                               :description "This study involves a retrospective medical record review, with a particular focus on pulmonary function tests."
                               :owner owner
                               :published t
                               :priority 1
                               :diary-p nil
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
              (make-question-named-and-numbered +survey-name-lam-history+ 1. "Country where patient receives medical care"
                                                ;; !! TODO: Country names from database !!
                                                :data-type :string))
             (q2
              (make-question-named-and-numbered +survey-name-lam-history+ 2. "Date of diagnosis of LAM or TSC-LAM"
                                                :data-type :date))
             (q3
              (make-question-named-and-numbered +survey-name-lam-history+ 3. "Age at time of diagnosis"
                                                :prompt-suffix "(years)" :data-type :number))
             (q4
              (apply #'make-question-named-and-numbered +survey-name-lam-history+ 4. "Diagnosis type"
                     (radio-options
                      '(("Sporadic LAM" . "LAM")
                        ("Tuberous Sclerosis Complex and LAM (TSC-LAM)" . "TSC-LAM")
                        ("Unknown" . "Unknown")))))
             ;; Rule: if q4 answer is TSC-LAM...
             (q5
              (apply #'make-question-named-and-numbered +survey-name-lam-history+ 5. "If the patient has TSC-LAM, what symptoms does the patient have"
                     :prompt-suffix "?<BR>Please check all that apply:"
                     (multi-choices-options
                      (choices-mirror-alist
                       '("Developmental delay" "Behavioral problem" "Seizures" "Other" "None" "Unknown")))))
             ;; Rule: if q5 answer included Other...
             (q5o
              (make-question "Other TSC-LAM symptoms" :data-type :string :view-type :text-field))
             ;; Rule: if q4 answer is TSC-LAM...
             (q6
              (apply #'make-question-named-and-numbered +survey-name-lam-history+ 6. "If the patient has TSC-LAM, what organ systems are affected by tumors"
                     :prompt-suffix "?<BR>Please check all that apply:"
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
      (let* ((q7
              (apply #'make-question-named-and-numbered +survey-name-lam-history+ 7  "How was LAM diagnosed"
                     :prompt-suffix "?"
                     (multi-choices-options
                      (choices-mirror-alist
                       '("Pathological diagnosis" "Clinical diagnosis without biopsy" "Other" "Unknown")))))
             (*group* (make-survey-group-named-and-numbered survey +survey-name-lam-history+ t :order (list q7)))
             ;; Rule: if q7 was pathological diagnosis
             (q7a
              (apply #'make-question "If pathological diagnosis, please indicate the type of biopsy"
                     :prompt-suffix "(check all that apply)"
                     (multi-choices-options
                      (choices-mirror-alist
                       '("Lung biopsy" "Biopsy of lymph node" "Biopsy of other mass")))))
             (q7a2
              (apply #'make-question "If lung biopsy, please specify type"
                     (radio-options
                      (choices-breaks-alist
                       '(("Open lung biopsy" . "open")
                         ("Video-assisted thorascopic surgery (VATS) biopsy" . "VATS")
                         ("Transbronchial lung biopsy" . "Transbronchial"))))))
             (q7b
              (apply #'make-question "If clinical diagnosis, what was used to make the diagnosis"
                     :prompt-suffix "? Please check all that apply:"
                     (multi-choices-options
                      (choices-breaks-alist
                       '("Chest CT" "Other imaging findings" "Cliniical picture" "Pulmonary function tests" "Other")))))
             ;; Rule: if q7b is "Other imaging findings"
             (q7bo1 (make-question "Other imaging findings" :prompt-suffix ". Please specify" :data-type :string))
             (q7bo2 (make-question "Other" :data-type :string))
             (subgroup7a (make-survey-sub-group-named *group* nil :order (list q7a)))
             (subgroup7a2 (make-survey-sub-group-named *group* nil :order (list q7a2)))
             (subgroup7b (make-survey-sub-group-named *group* nil :order (list q7b)))
             (subgroup7bo1 (make-survey-sub-group-named *group* nil :order (list q7bo1)))
             (subgroup7bo2 (make-survey-sub-group-named *group* nil :order (list q7bo2)))
             (q7c (make-question "Other diagnosis" :prompt-suffix "please specify" :data-type :string))
             (subgroup7c (make-survey-sub-group-named *group* nil :order (list q7c))))
        ;; Group rules
        (add-rule *group* q7 "Pathological diagnosis" subgroup7a ':inline)
        (add-rule subgroup7a q7a "Lung biopsy" subgroup7a2 ':inline)
        (add-rule *group* q7 "Clinical diagnosis without biopsy" subgroup7b ':inline)
        (add-rule subgroup7b q7b "Other imaging findings" subgroup7bo1 ':inline)
        (add-rule subgroup7b q7b "Other" subgroup7bo2 ':inline)
        (add-rule *group* q7 "Other" subgroup7c ':inline))

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
              (apply #'make-question-named-and-numbered +survey-name-lam-history+ 9. "How did the patient originally present"
                     :prompt-suffix "? What symptom, finding or event led to the eventual diagnosis of LAM?"
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
              (make-question-named-and-numbered +survey-name-lam-history+ 10. "How old was the patient at time of the first symptoms attributed to LAM"
                                                :prompt-suffix "? (years)" :data-type :number))
             (q11
              (apply #'make-question-named-and-numbered +survey-name-lam-history+ 11. "What is the patient's smoking history"
                     :prompt-suffix "?"
                     (radio-options
                      (choices-breaks-alist
                       '("Current smoker" "Former smoker" "Never a smoker" "Unknown")))))
             (q11a
              (make-question "On average the patient smokes how many packs per day"
                             :prompt-suffix "?" :data-type :number))
             (q11b
              (make-question "How many years has the patient smoked"
                             :prompt "How many years has the patient smoked?" :data-type :number))
             (q11c
              (make-question "Total pack years"
                             :prompt "<B>Or</B> enter total pack years:" :data-type :number))
             (q12
              (apply #'make-question-named-and-numbered +survey-name-lam-history+ 12. "Has the patient ever had a pneumothorax"
                     :prompt-suffix "?"
                     (radio-options
                      (choices-mirror-alist
                       '("Yes" "No" "Unknown")))))
             (q12a
              (make-question "If yes how many total" :prompt-suffix "?" :data-type :number))
             (q12b
              (make-question "If yes how many in the past year" :prompt-suffix "?" :data-type :number))
             (q13
              (apply #'make-question-named-and-numbered +survey-name-lam-history+ 13. "Has the patient ever had a pleural effusion"
                     :prompt-suffix "?"
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
              (apply #'make-question-named-and-numbered +survey-name-lam-history+ 14. "What extrapulmonary lesions does the patient have"
                     :prompt-suffix "?<BR>Please check all that apply:"
                     (multi-choices-options
                      (choices-mirror-alist
                       '("Renal angiomyolipoma" "Non-renal angiomyolipoma" "Lymphangiomyoma"
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
             (q14b2 (make-question "Other"))                  
             ;;Is it necessary to avoid re-cycling the previous question??
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
              (make-question-named-and-numbered +survey-name-lam-history+ 15. "What was the patient's age of menarche"
                                                :prompt-suffix "? (years)"
                                                ;; ?? TODO: "Unknown" checkbox for Q15 ??
                                                :data-type :number))
             (q16
              (apply #'make-question-named-and-numbered +survey-name-lam-history+ 16. "Did the patient take oral contraceptive pills before diagnosed with LAM"
                     :prompt-suffix "?"
                     (radio-options
                      (choices-mirror-alist
                       '("Yes" "No" "Unknown")))))
             (q16a
              (make-question "Please specify type of contraceptive" :data-type :string))
             (q16b
              (make-question "Dates of use" :data-type :date-range))
             (q17
              (apply #'make-question-named-and-numbered +survey-name-lam-history+ 17. "Has the patient ever been pregnant"
                     (radio-options
                      (choices-mirror-alist
                       '("Yes" "No" "Unknown")))))
             (q17-table
              (make-survey-group-table (:name "question 17 if yes" :advice "If yes: please complete:" :default-data-type :number)
                                       ( nil "Number <B>before<B> diagnosis of LAM"
                                             "Number <B>during</B> or <B>after</b> diagnosis of LAM" )
                                       ( "Full term or premature births" (:question) (:question) )
                                       ( "Miscarriages"  (:question) (:question) )
                                       ( "Abortions" (:question) (:question) )))

             (q18
              (apply #'make-question-named-and-numbered +survey-name-lam-history+ 18. "Has the patient gone through menopause"
                     :prompt-suffix "? (years)"
                     (radio-options
                      (choices-mirror-alist
                       '("Yes" "No" "Unknown")))))
             (q18a
              (make-question "age at time of menopause" :prompt-prefix "If yes, " :prompt-suffix "?" :data-type :number))
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
                                                    :advice "<SUP>19</SUP> What is the patient's LAM related <B>treatment</B> history? Please check all that apply:"))
             (*questions*
              (loop for spec in
                   `(("No treatment")
                     ("Hormone therapy"
                      nil
                      (("Gn-RH agonist" :date-range)
                       ("Progesterone" :date-range)
                       ("Tamoxifen" :date-range)
                       ("Surgical oophorectomy" :date)
                       ("Other" :date-range :other)))
                     ("Bronchiectasis treatment"
                      nil
                      (("Long acting B agonist" :date-range)
                       ("Oral B agonist" :date-range)
                       ("Transdermal B agonist" :date-range)
                       ("Anti-cholinergic" :date-range)
                       ("Aminophylline" :date-range)
                       ("Inhaled steroids" :date-range)
                       ("Other" :date-range :other))
                      ("Other medical treatment"
                       nil
                       ("Sirolimus/Rapamune" :date-range)
                       ("Other" :date-range :other)))
                     ("Pneumothorax or pleural effusion treatment"
                      nil
                      (("Chest tube placement / pleural drainage.")
                       ("Open chest surgery")
                       ("Thorascopic / minimally invasive chest surgery")
                       ("Pleurodesis")
                       ("Other" :other)))
                     ("Other surgery"
                      nil
                      (("Thoracic duct ligation" :date)
                       ("Nephrectomy" :date)
                       ("Hysterectomy" :date)
                       ("Other" :date :other)))
                     ("Transplant" 
                      ,(radio-options
                        (choices-breaks-alist
                         '(("The Patient has required transplant evaluation" . "none")
                           ("The Patient was evaluated, but has not had a transplant" . "evaluated")
                           ("The patient had a transplant" . "transplant"))))))
                   as name19a = (first spec)
                   as options19a = (second spec)
                   as q19a = (apply #'make-question name19a :prompt name19a
                                    (or options19a (choices-options-yes-no)))
                   as specs19b = (third spec)
                   collect
                   (let* ((subgroup19a (make-survey-sub-group-named *group* nil))
                          (qs19b
                           (loop for spec19b in specs19b
                              as name19b = (first spec19b)
                              as other-p = (member ':other (rest spec19b))
                              as date-range-p = (member ':date-range (rest spec19b))
                              as q19b = (apply #'make-question name19b (choices-options-yes-no))
                              as q19c =
                              (make-question (format nil "~A date" name19b)
                                             :data-type
                                             ;; !! TODO: generalize this !!
                                             (if date-range-p ':date-range ':date))
                              as subgroup19b = (make-survey-sub-group-named *group* nil :order (list q19c))
                              collect
                              (progn
                                ;; Group rules
                                (when other-p
                                  (let ((q19b-other
                                         (make-question "Other, please specify" :data-type :string)))
                                    (push q19b-other (group-questions subgroup19b))
                                    (add-rule *group* q19b t subgroup19b ':inline)))
                                (add-rule subgroup19a q19b t subgroup19b ':inline)
                                ;; Returns
                                q19b))))
                     ;; Questions for group
                     (setf (group-questions subgroup19a) qs19b)
                     ;; Group rules
                     (add-rule *group* q19a t subgroup19a ':inline)
                     ;; Returns
                     q19a))))
        (setf (group-questions *group*) *questions*))

      ;;
      ;; Group 9
      ;;
      (let* ((q20
              (apply #'make-question-named-and-numbered +survey-name-lam-history+ 20. "Does/did the patient use oxygen at home"
                     :prompt-suffix "?"
                     (choices-options-yes-no)))
             (*group* (make-survey-group-named-and-numbered survey +survey-name-lam-history+ t :order (list q20)))
             (q20a
              (apply #'make-question "If yes the patient uses oxygen at home"
                     :prompt "If yes:"
                     (multi-choices-options
                      (choices-mirror-alist
                       '("Continuous" "With activities" "At night" "Other")))))
             (q20b (make-question "Average #liters/min" :data-type :number))
             (q20c
              (make-question "When did the patient initiate home oxygen"
                             :prompt-suffix "?"
                             :data-type :date))
             (subgroup20
              (make-survey-sub-group-named *group* nil :order (list q20a q20b q20c))))
        ;; Group rules
        (add-rule *group* q20 t subgroup20 ':successor))

      ;;
      ;; Group 10
      ;;
      (let* ((q21
              (apply #'make-question-named-and-numbered +survey-name-lam-history+ 21. "What is the patient's vital status"
                     :prompt-suffix "?"
                     (radio-options
                      (choices-mirror-alist
                       '("Living" "Deceased")))))
             (*group* (make-survey-group-named-and-numbered survey +survey-name-lam-history+ t :order (list q21)))
             (q21a (make-question "Date of last confirmation" :data-type :date))
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
              (apply #'make-question-named-and-numbered +survey-name-lam-history+ 22. "Do you have pulmonary function test (PFT) reports for the patient"
                     :prompt-suffix "?"
                     (choices-options-yes-no)))
             (q22-confirm
              (apply #'make-question "Confirm that all PFT results have been entered for the patient"
                     :prompt-prefix "Please enter <B>all</B> the results that you have for the patient on the separate PFT patient diary.<BR>"
                     (choices-options-yes-no)))
             (q23
              (apply #'make-question-named-and-numbered +survey-name-lam-history+ 23. "Does the patient have <B>six minute walk distance</B> (6MWD) results"
                     :prompt-suffix "?"
                     (choices-options-yes-no)))
             (q23-confirm
              (apply #'make-question "Confirm that all 6MWD results have been entered for the patient"
                     :prompt-prefix "Please enter <B>all</B> the results that you have for the patient on the separate 6MWD patient diary.<BR>"
                     (choices-options-yes-no)))
             (q24
              (apply #'make-question-named-and-numbered +survey-name-lam-history+ 24. "Has the patient taken the <B>St. George's Respiratory Questionnaire (SGRQ)</B>"
                     :prompt-suffix "?"
                     (choices-options-yes-no)))
             (q24-confirm
              (apply #'make-question "Confirm that all SGRQ results have been entered for the patient"
                     :prompt-prefix "Please enter <B>all</B> the results that you have for the patient on the separate SGRQ patient diary.<BR>"
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
      ;; Survey 2 - PFT diary
      ;;
      (flet ((make-pft-questions ()
               (loop for spec in
                    '(("Vital Capacity (VC)" "ml.")
                      ("Forced Vital Capacity (FVC)" "ml.")
                      ("Forced Expiratory Volume in 1 sec (FEV1)" "ml.")
                      ("Carbon Monoxide Diffusing Capacity (DLCO)" "ml/min/mm Hg.")
                      ("DLCO/(alveolar volume)(V<SUB>A</SUB>))" "ml/min/mm Hg/L.")
                      ("Total Lung Capacity (TLC)" "ml." ("Body box method" "Washing method"))
                      ("Residual Volume (RV)" "ml." ("Body box method" "Washing method")))
                    as qt1-name = (first spec)
                    as qt1-units = (second spec)
                    as qt4-choices = (third spec)
                    append
                    (let* ((qt1
                            (make-question (format nil "~A - Result" qt1-name)
                                           :prompt-prefix "<HR>"
                                           :help qt1-units :data-type :number))
                           (qt2
                            (make-question (format nil "~A - Percent Predicted" qt1-name)
                                           :prompt "Percent Predicted:"
                                           :help "(%)" :data-type :number))
                           (qt3
                            (apply #'make-question (format nil "~A - Test not performed" qt1-name)
                                   :prompt "Test not performed:"
                                   (choices-options-yes-no)))
                           (qt4
                            (and qt4-choices
                                 (apply #'make-question (format nil "~A method" qt1-name)
                                        (radio-options
                                         (choices-breaks-alist qt4-choices))))))
                      ;; Returns
                      (append (list qt1 qt2 qt3) (if qt4 (list qt4)))))))
        (let* ((q1
                (make-question "Date of Pulmnonary Function Test" :data-type :date))
               (q2
                (apply #'make-question "Please check if the patient had any of the following at the time of this PFT result"
                       (multi-choices-options
                        (choices-breaks-alist
                         '("Pneumothorax" "Pleural effusion" "Pneumonia" "Chest surgery within the previous 6 months" "Unknown")))))
               ;; Q3 is really a table of questions
               (q3s (make-pft-questions))
               (group1
                (make-survey-group-named-and-numbered survey-pft "LAM History PFT" nil
                                                      :order (append (list q1 q2) q3s)))
               ;; Second set, repeat of questions post-bronchodilator
               (q4
                (apply #'make-question "Were tests performed <B>POST-BRONCHODILATOR</B>"
                       :prompt-suffix " (examples: albuterol or ipratroprium inhaler/nebulizer)?"
                       (choices-options-yes-no)))
               (group2
                (make-survey-group-named-and-numbered survey-pft "LAM History PFT" nil
                                                      :order (append (list q4))))
               (q4subgroup
                (make-survey-sub-group-named group2 nil :order (make-pft-questions))))
          (declare (ignore group1))
          ;; Group rules
          (add-rule group2 q4 t q4subgroup ':successor)
          ;; Survey diary question
          (setf (diary-question survey-pft) q1)))

      ;;
      ;; Survey 3 - 6MWD
      ;;
      (let* ((q1 (make-question "Date" :data-type :date))
             (q2 (make-question "Result" :prompt-suffix " (feet/meters)" :data-type :number))
             (*group*
              (make-survey-group-named-and-numbered survey-6mwd "LAM History 6MWD" nil :order (list q1 q2))))
        (declare (ignore *group*))
        ;; Survey diary question
        (setf (diary-question survey-6mwd) q1))

      ;;
      ;; Survey 4 - SGRQ
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
      (list survey survey-pft survey-6mwd survey-sgrq))))

(defun create-lam-history-data (&key (count 10.) (center "lamhtest") #+NIL (owner (current-user)))
  (let ((questions (gethash +survey-name-lam-history+ *survey-question-table*)))
    (assert (not (null questions)) nil "Survey questions not found for ~S" +survey-name-lam-history+)
    (with-transaction ()
      ;; Coerce center
      (when (stringp center)
        (setq center
              (or (get-center center t)
                  (make-center center "LAM History survey - test center"))))
      (check-type center center)
      ;; Coerce owner / user
      #+NIL (when (stringp owner) (setf owner (get-user owner)))
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
          
          ))))
    ;; Done
    ))

(defun create-lam-history-report (&key (questions '(3 4 5 6 7 9 10 15)) (center "lamhtest")
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
    
