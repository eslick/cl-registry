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

(defvar *default-study-owner*)
(defvar *survey-group-counter*)

(defun create-lam-history (&key (owner (current-user)))
  (with-transaction ()
    (let* ((*question-prompt-suffix* ":")
           (survey
            (make-instance 'survey
                           :name "LAM History"
                           :description "This study involves a retrospective medical record review, with a particular focus on pulmonary function tests."
                           :owner owner
                           :published t
                           :priority 1
                           :diary-p nil
                           :ranking-record (make-ranking-record :ranking nil :distribution nil))))

      ;;
      ;; Group 1 - clinician header page
      ;;
      (make-survey-group-named-and-numbered survey "LAM History" t
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
              (make-question-named-and-numbered 1. "Country where patient receives medical care"
                                                ;; !! TODO: Country names from database !!
                                                :data-type :string))
             (q2
              (make-question-named-and-numbered 2. "Date of diagnosis of LAM or TSC-LAM"
                                                :data-type :date))
             (q3
              (make-question-named-and-numbered 3. "Age at time of diagnosis" :prompt-suffix "(years)" :data-type :number))
             (q4
              (apply #'make-question-named-and-numbered 4. "Diagnosis type"
                     (radio-options
                      '(("Sporadic LAM" . "LAM")
                        ("Tuberous Sclerosis Complex and LAM (TSC-LAM)" . "TSC-LAM")
                        ("Unknown" . "Unknown")))))
             ;; Rule: if q4 answer is TSC-LAM...
             (q5
              (apply #'make-question-named-and-numbered 5. "If the patient has TSC-LAM, what symptoms does the patient have"
                     :prompt-suffix "?<BR>Please check all that apply:"
                     (multi-choices-options
                      (choices-mirror-alist
                       '("Developmental delay" "Behavioral problem" "Seizures" "Other" "None" "Unknown")))))
             ;; Rule: if q5 answer included Other...
             (q5o
              (make-question "Other TSC-LAM symptoms" :data-type :string :view-type :text-field))
             ;; Rule: if q4 answer is TSC-LAM...
             (q6
              (apply #'make-question-named-and-numbered 6. "If the patient has TSC-LAM, what organ systems are affected by tumors"
                     :prompt-suffix "?<BR>Please check all that apply:"
                     (multi-choices-options
                      (choices-mirror-alist
                       '("Brain" "Kidneys" "Heart" "Eyes" "Skin" "Lungs" "Other" "Unknown/Not screened")))))
             ;; Rule: if previous answer included Other...
             (q6o
              (make-question "Other TSC-LAM symptoms" :data-type :string :view-type :text-field))
             (*group* (make-survey-group-named-and-numbered survey "LAM History" t :order (list q1 q2 q3 q4)))
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
              (apply #'make-question-named-and-numbered 7  "How was LAM diagnosed"
                     :prompt-suffix "?"
                     (multi-choices-options
                      (choices-mirror-alist
                       '("Pathological diagnosis" "Clinical diagnosis without biopsy" "Other" "Unknown")))))
             (*group* (make-survey-group-named-and-numbered survey "LAM History" t :order (list q7)))
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
              (make-survey-group-named-and-numbered survey "LAM History" t
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
              (apply #'make-question-named-and-numbered 9. "How did the patient originally present"
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
              (make-question-named-and-numbered 10. "How old was the patient at time of the first symptoms attributed to LAM"
                                                :prompt-suffix "? (years)" :data-type :number))
             (q11
              (apply #'make-question-named-and-numbered 11. "What is the patient's smoking history"
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
              (apply #'make-question-named-and-numbered 12. "Has the patient ever had a pneumothorax"
                     :prompt-suffix "?"
                     (radio-options
                      (choices-mirror-alist
                       '("Yes" "No" "Unknown")))))
             (q12a
              (make-question "If yes how many total" :prompt-suffix "?" :data-type :number))
             (q12b
              (make-question "If yes how many in the past year" :prompt-suffix "?" :data-type :number))
             (q13
              (apply #'make-question-named-and-numbered 13. "Has the patient ever had a pleural effusion"
                     :prompt-suffix "?"
                     (radio-options
                      (choices-mirror-alist
                       '("Yes" "No" "Unknown")))))
             (*group*
              (make-survey-group-named-and-numbered survey "LAM History" t :order (list q9 q10 q11 q12 q13)))
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
              (apply #'make-question-named-and-numbered 14. "What extrapulmonary lesions does the patient have"
                     :prompt-suffix "?<BR>Please check all that apply:"
                     (multi-choices-options
                      (choices-mirror-alist
                       '("Renal angiomyolipoma" "Non-renal angiomyolipoma" "Lymphangiomyoma"
                         "Chylous ascites" "Chylous pleural effusion"
                         "Other" "None")))))
             (*group* (make-survey-group-named-and-numbered survey "LAM History" t :order (list q14)))
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
              (make-question-named-and-numbered 15. "What was the patient's age of menarche"
                                                :prompt-suffix "? (years)"
                                                ;; ?? TODO: "Unknown" checkbox for Q15 ??
                                                :data-type :number))
             (q16
              (apply #'make-question-named-and-numbered 16. "Did the patient take oral contraceptive pills before diagnosed with LAM"
                     :prompt-suffix "?"
                     (radio-options
                      (choices-mirror-alist
                       '("Yes" "No" "Unknown")))))
             (q16b
              (make-instance 'survey-group
                             :name "Dates of contraceptive use."
                             :advice ""
                             :order (list (make-question "Please specify type of contraceptive" :data-type :string)
                                          (make-question "Dates of use" :data-type :date-range))))
             (q17
              (apply #'make-question-named-and-numbered 17. "Has the patient ever been pregnant"
                     (radio-options
                      (choices-mirror-alist
                       '("Yes" "No" "Unknown")))))
             (q17-table
              (make-survey-group-table (:name "q17 related questions" :advice "If yes please complete:" :default-data-type :number)
                                       ( nil "Number before diagnosis" "Number after diagnosis" )
                                       ( "Full term or premature births" (:question) (:question) )
                                       ( "Miscarriages"  (:question) (:question) )
                                       ( "Abortions" (:question) (:question) )))

             (q18
              (apply #'make-question-named-and-numbered 18. "Has the patient gone through menopause"
                     :prompt-suffix "?"
                     (radio-options
                      (choices-mirror-alist
                       '("Yes" "No" "Unknown")))))
             (*group*
              (make-survey-group-named-and-numbered survey "LAM History" t :order (list q15 q16 q17 q18))))
        ;; Group rules
        (add-rule *group* q16 "Yes" q16b :inline)
        (add-rule *group* q17 "Yes" q17-table :inline))

      ;;
      ;; Group 8
      ;;
      (let* ((*group*
              (make-survey-group-named-and-numbered survey "LAM History" t
                                                    :advice "<SUP>19</SUP> What is the patient's LAM related <B>treatment</B> history? Please check all that apply:"))
             (questions20
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
                   as name20a = (first spec)
                   as options20a = (second spec)
                   as q20a = (apply #'make-question name20a :prompt name20a
                                    (or options20a (choices-options-yes-no)))
                   as specs20b = (third spec)
                   collect
                   (let* ((subgroup20a (make-survey-sub-group-named *group* nil))
                          (qs20b
                           (loop for spec20b in specs20b
                              as name20b = (first spec20b)
                              as other-p = (member ':other (rest spec20b))
                              as date-range-p = (member ':date-range (rest spec20b))
                              as q20b = (apply #'make-question name20b (choices-options-yes-no))
                              as q20c =
                              (make-question (format nil "~A date" name20b)
                                             :data-type
                                             ;; !! TODO: generalize this !!
                                             (if date-range-p ':date-range ':date))
                              as subgroup20b = (make-survey-sub-group-named *group* nil :order (list q20c))
                              collect
                              (progn
                                ;; Group rules
                                (when other-p
                                  (let ((q20b-other
                                         (make-question "Other, please specify" :data-type :string)))
                                    (push q20b-other (group-questions subgroup20b))
                                    (add-rule *group* q20b t subgroup20b ':inline)))
                                (add-rule subgroup20a q20b t subgroup20b ':inline)
                                ;; Returns
                                q20b))))
                     ;; Questions for group
                     (setf (group-questions subgroup20a) qs20b)
                     ;; Group rules
                     (add-rule *group* q20a t subgroup20a ':inline)
                     ;; Returns
                     q20a))))
        (setf (group-questions *group*) questions20))

      ;;
      ;; Group 9
      ;;
      (let* ((q21
              (apply #'make-question-named-and-numbered 20. "Does/did the patient use oxygen at home"
                     :prompt-suffix "?"
                     (choices-options-yes-no)))
             (*group* (make-survey-group-named-and-numbered survey "LAM History" t :order (list q21)))
             (q21a
              (apply #'make-question "If yes the patient uses oxygen at home"
                     :prompt "If yes:"
                     (multi-choices-options
                      (choices-mirror-alist
                       '("Continuous" "With activities" "At night" "Other")))))
             (q21b (make-question "Average #liters/min" :data-type :number))
             (q21c
              (make-question "When did the patient initiate home oxygen"
                             :prompt-suffix "?"
                             :data-type :date))
             (subgroup21
              (make-survey-sub-group-named *group* nil :order (list q21a q21b q21c))))
        ;; Group rules
        (add-rule *group* q21 t subgroup21 ':successor))

      ;;
      ;; Group 10
      ;;
      (let* ((q22
              (apply #'make-question-named-and-numbered 21. "What is the patient's vital status"
                     :prompt-suffix "?"
                     (radio-options
                      (choices-mirror-alist
                       '("Living" "Deceased")))))
             (*group* (make-survey-group-named-and-numbered survey "LAM History" t :order (list q22)))
             (q22a (make-question "Date of last confirmation" :data-type :date))
             (subgroup22-living (make-survey-sub-group-named *group* nil :order (list q22a)))
             (q22b (make-question "Date of death" :data-type :date))
             (q22c
              (apply #'make-question "What was the cause of death"
                     :prompt-suffix "?"
                     (radio-options
                      (choices-breaks-alist
                       '("Respiratory failure" "Infection" "Pulmonary thromboembolism" "Cancer" "Other")))))
             (subgroup22-dead (make-survey-sub-group-named *group* nil :order (list q22b q22c)))
             (q22d (make-question "Please indicate type of infection if known" :data-type :string))
             (subgroup22-infection (make-survey-sub-group-named *group* nil :order (list q22d)))
             (q22e (make-question "Please indicate type of cancer if known" :data-type :string))
             (subgroup22-cancer (make-survey-sub-group-named *group* nil :order (list q22e))))
        ;; Group rules
        (add-rule *group* q22 "Living" subgroup22-living ':inline)
        (add-rule *group* q22 "Deceased" subgroup22-dead ':inline)
        (add-rule subgroup22-dead q22c "Infection" subgroup22-infection :inline)
        (add-rule subgroup22-dead q22c "Cancer" subgroup22-cancer :inline))
        
      ;; Returns
      survey)))
