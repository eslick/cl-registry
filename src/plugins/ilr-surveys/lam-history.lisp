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
    (flet ((lmake-survey-page-group (&rest args)
             (apply #'make-instance 'survey-group
                    :owner *default-study-owner*
                    :name (format nil "LAM History Part ~D" (incf *survey-group-counter*))
                    args))
           (lmake-survey-sub-group (&rest args)
             (apply #'make-instance 'survey-group :name (symbol-name (gensym)) :owner *default-study-owner* args))
           (lmake-question (num name &rest args)
             (apply #'make-question name :prompt-prefix (format nil "<SUP>~D</SUP>" num) args)))
      (let* ((*default-study-owner* owner)
             (*survey-group-counter* 0.)
             (*question-prompt-suffix* ":")
             (*group0
              (lmake-survey-page-group
               :advice "<B>Clinician Header</B>
<P>Thank you for your involvement in this research study and your collaboration with the International LAM Registry!
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
<P> Additionally, please encourage all of your LAM patients to register on LAMsight.org!"))
             (*group1
              (let* (
                     #|
                     (q1
                      (lmake-question 1. "Patient name" :data-type :string))
                     (q2
                      (lmake-question 2. "Patient date of birth" :data-type :date))
                     |#
                     (q3
                      (lmake-question 1. "Country where patient receives medical care"
                                      ;; !! TODO: Country names from database !!
                                      :data-type :string))
                     (q4
                      (lmake-question 2. "Date of diagnosis of LAM or TSC-LAM"
                                      :data-type :date))
                     (q3i
                      (lmake-question 3. "Age at time of diagnosis" :prompt-suffix "(years)" :data-type :number))
                     (q5
                      (apply #'lmake-question 4. "Diagnosis type"
                             (radio-options
                              '(("Sporadic LAM" . "LAM")
                                ("Tuberous Sclerosis Complex and LAM (TSC-LAM)" . "TSC-LAM")
                                ("Unknown" . "Unknown")))))
                     ;; Rule: if q5 answer is TSC-LAM...
                     (q6
                      (apply #'lmake-question 5. "If the patient has TSC-LAM, what symptoms does the patient have"
                             :prompt-suffix "?<BR>Please check all that apply:"
                             (multi-choices-options
                              (choices-mirror-alist
                               '("None" "Developmental delay" "Behavioral problem" "Other" "Unknown")))))
                     ;; Rule: if q6 answer included Other...
                     (q6o
                      (make-question "Other TSC-LAM symptoms" :data-type :string :view-type :text-field))
                     ;; Rule: if q5 answer is TSC-LAM...
                     (q7
                      (apply #'lmake-question 6. "If the patient has TSC-LAM, what organ systems are affected by tumors"
                             :prompt-suffix "?<BR>Please check all that apply:"
                             (multi-choices-options
                              (choices-mirror-alist
                               '("Kidneys" "Heart" "Eyes" "Skin" "Lungs" "Other" "Unknown")))))
                     ;; Rule: if previous answer included Other...
                     (q7o
                      (make-question "Other TSC-LAM symptoms" :data-type :string :view-type :text-field))
                     (questions1 (list q3 q4 q3i q5))
                     (group1 (lmake-survey-page-group :order questions1))
                     ;; Subgroups
                     (subgroup1 (lmake-survey-sub-group :order (list q6 q7)))
                     (subgroup2 (lmake-survey-sub-group :order (list q6o)))
                     (subgroup3 (lmake-survey-sub-group :order (list q7o))))
                ;; Group rules
                (add-rule group1 q5 "TSC-LAM" subgroup1 ':successor)
                (add-rule group1 q6 "Other" subgroup2 ':successor)
                (add-rule group1 q7 "Other" subgroup3 ':successor)
                ;; Returns
                group1))
             ;; How was LAM diagnosed?
             (*group8
              (let* ((q8
                      (apply #'lmake-question 7  "How was LAM diagnosed"
                             :prompt-suffix "?"
                             (multi-choices-options
                              (choices-mirror-alist
                               '("Pathological diagnosis" "Clinical diagnosis without biopsy" "Other" "Unknown")))))
                     (questions8 (list q8))
                     (group8 (lmake-survey-page-group :order questions8))
                     ;; Rule: if q8 was pathological diagnosis
                     (q8a
                      (apply #'make-question "If pathological diagnosis, please indicate the type of biopsy"
                             :prompt-suffix "(check all that apply)"
                             (multi-choices-options
                              (choices-mirror-alist
                               '("Lung biopsy" "Biopsy of lymph node" "Biopsy of other mass")))))
                     (q8a2
                      (apply #'make-question "If lung biopsy, please specify type"
                             (radio-options
                              (choices-breaks-alist
                               '(("Open lung biopsy" . "open")
                                 ("Video-assisted thorascopic surgery (VATS) biopsy" . "VATS")
                                 ("Transbronchial lung biopsy" . "Transbronchial"))))))
                     (q8b
                      (apply #'make-question "If clinical diagnosis, what was used to make the diagnosis"
                             :prompt-suffix "? Please check all that apply:"
                             (multi-choices-options
                              (choices-breaks-alist
                               '("Chest CT" "Other imaging findings" "Cliniical picture" "Pulmonary function tests" "Other")))))
                     ;; Rule: if q8b is "Other imaging findings"
                     (q8bo1 (make-question "Other imaging findings" :prompt-suffix ". Please specify" :data-type :string))
                     (q8bo2 (make-question "Other" :data-type :string))
                     (subgroup8a (lmake-survey-sub-group :order (list q8a)))
                     (subgroup8a2 (lmake-survey-sub-group :order (list q8a2)))
                     (subgroup8b (lmake-survey-sub-group :order (list q8b)))
                     (subgroup8bo1 (lmake-survey-sub-group :order (list q8bo1)))
                     (subgroup8bo2 (lmake-survey-sub-group :order (list q8bo2)))
                     (q8c (make-question "Other diagnosis" :prompt-suffix "please specify" :data-type :string))
                     (subgroup8c (lmake-survey-sub-group :order (list q8c))))
                ;; Group rules
                (add-rule group8 q8 "Pathological diagnosis" subgroup8a ':inline)
                (add-rule subgroup8a q8a "Lung biopsy" subgroup8a2 ':inline)
                (add-rule group8 q8 "Clinical diagnosis without biopsy" subgroup8b ':inline)
                (add-rule subgroup8b q8b "Other imaging findings" subgroup8bo1 ':inline)
                (add-rule subgroup8b q8b "Other" subgroup8bo2 ':inline)
                (add-rule group8 q8 "Other" subgroup8c ':inline)
                ;; Returns
                group8))
             ;; Q9 outline
             ;; ?? TODO: Should q9 be related to q8 by rule ??
             (*group9
              (let* ((group9
                      (lmake-survey-page-group
                       :advice "<SUP>8</SUP> If a biopsy was performed, what were the histopathological findings?"))
                     (questions
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
                                (let* ((question2
                                        (apply #'make-question (format nil "~A ~A" name (gensym))
                                               :prompt nil
                                               (radio-options (choices-breaks-alist choices))))
                                       (subgroup (lmake-survey-sub-group :order (list question2))))
                                  ;; !! TODO: Deal with "Other" choice !!
                                  (add-rule group9 question value subgroup ':inline)))
                             ;; Return 
                             question))))
                (setf (group-questions group9) questions)
                ;; Returns
                group9))
             (*group10
              (let* ((q10
                      (apply #'lmake-question 9. "How did the patient originally present"
                             :prompt-suffix "? What symptom, finding or event led to the eventual diagnosis of LAM?"
                             (multi-choices-options
                              (choices-mirror-alist
                               '("Exertional dyspnea"
                                 "Spontaneous pneumothorax"
                                 "Pleural effusion"
                                 "Chylous ascites"
                                 "Other")))))
                     ;; Rule: if q10 included Other...
                     (q10o
                      (make-question "Other symptom(s) originally presented" :data-type :string))
                     (q11
                      (lmake-question 10. "How old was the patient at time of the first symptoms attributed to LAM"
                                      :data-type :number))
                     (q12
                      (apply #'lmake-question 11. "What is the patient's smoking history"
                             :prompt-suffix "?"
                             (radio-options
                              (choices-breaks-alist
                               '("Current smoker" "Former smoker" "Never a smoker" "Unknown")))))
                     (q13
                      (apply #'lmake-question 12. "Has the patient ever had a pneumothorax"
                             :prompt-suffix "?"
                             (radio-options
                              (choices-mirror-alist
                               '("Yes" "No" "Unknown")))))
                     (q14
                      (apply #'lmake-question 13. "Has the patient ever had a pleural effusion"
                             :prompt-suffix "?"
                             (radio-options
                              (choices-mirror-alist
                               '("Yes" "No" "Unknown")))))
                     (questions10 (list q10 q11 q12 q13 q14))
                     (group10
                      (lmake-survey-page-group :order questions10))
                     (subgroup10o (lmake-survey-sub-group :order (list q10o))))
                ;; Group rule
                (add-rule group10 q10 "Other" subgroup10o ':inline)
                ;; Returns
                group10))
             (*group15
              (let* ((q15
                      (apply #'lmake-question 14. "What extrapulmonary lesions does the patient have"
                             :prompt-suffix "?<BR>Please check all that apply:"
                             (multi-choices-options
                              (choices-mirror-alist
                               '("None" "Renal angiomyolipoma" "Non-renal angiomyolipoma" "Lymphangiomyoma"
                                 "Axillary lymph node swelling" "Chylous ascites" "Chylous pleural effusion"
                                 "Other")))))
                     (group15 (lmake-survey-page-group :order (list q15)))
                     (q15o (make-question "Other lesions" :data-type :string))
                     (subgroup1 (lmake-survey-sub-group :order (list q15o))))
                ;; Group rule
                (add-rule group15 q15 "Other" subgroup1 ':inline)
                ;; Returns
                group15))
             (*group16
              (let* ((q16
                      (lmake-question 15. "What was the patient's age of menarche"
                                      :prompt-suffix "?"
                                      ;; ?? TODO: "Unknown" checkbox for Q16 ??
                                      :data-type :number))
                     (q17
                      (apply #'lmake-question 16. "Did the patient take oral contraceptive pills before diagnosed with LAM"
                             :prompt-suffix "?"
                             (radio-options
                              (choices-mirror-alist
                               '("Yes" "No" "Unknown")))))
                     (q17b
                      (make-instance 'survey-group
                                     :name "Dates of contraceptive use."
                                     :advice ""
                                     :order (list (make-question "Please specify type of contraceptive" :data-type :string)
                                                  (make-question "Dates of use" :data-type :date-range))))
                     (q18
                      (apply #'lmake-question 17. "Has the patient ever been pregnant"
                             (radio-options
                              (choices-mirror-alist
                               '("Yes" "No" "Unknown")))))
                     (q18-table
                      (make-survey-group-table (:name "q18 related questions" :advice "If yes please complete:" :default-data-type :number)
                                               ( nil "Number before diagnosis" "Number after diagnosis" )
                                               ( "Full term or premature births" (:question) (:question) )
                                               ( "Miscarriages"  (:question) (:question) )
                                               ( "Abortions" (:question) (:question) )))

                     (q19
                      (apply #'lmake-question 18. "Has the patient gone through menopause"
                             :prompt-suffix "?"
                             (radio-options
                              (choices-mirror-alist
                               '("Yes" "No" "Unknown")))))
                     (questions16 (list q16 q17 q18 q19))
                     (group16
                      (lmake-survey-page-group :order questions16)))
                ;; Group rules
                (add-rule group16 q17 "Yes" q17b :inline)
                (add-rule group16 q18 "Yes" q18-table :inline)
                ;; Returns
                group16))
             (*group20
              (let* ((group20
                      (lmake-survey-page-group :advice "<SUP>19</SUP> What is the patient's LAM related <B>treatment</B> history?
Please check all that apply:"))
                     (questions20
                      (loop for spec in
                           `(("No treatment")
                             ("Hormone therapy" nil
                                                (("Gn-RH agonist" :date-range)
                                                 ("Progesterone" :date-range)
                                                 ("Tamoxifen" :date-range)
                                                 ("Surgical oophorectomy" :date)
                                                 ("Other" :date-range :other)))
                             ("Bronchiectasis treatment" nil
                                                         (("Long acting B agonist" :date-range)
                                                          ("Oral B agonist" :date-range)
                                                          ("Transdermal B agonist" :date-range)
                                                          ("Anti-cholinergic" :date-range)
                                                          ("Aminophylline" :date-range)
                                                          ("Inhaled steroids" :date-range)
                                                          ("Other" :date-range :other))
                                                         ("Other medical treatment" nil
                                                                                    ("Simolimus/Rapamune" :date-range)
                                                                                    ("Other" :date-range :other)))
                             ("Pneumothorax or pleural effusion treatment" nil
                                                                           (("Chest tube placement / pleural drainage.")
                                                                            ("Open chest surgery")
                                                                            ("Thorascopic / minimally invasive chest surgery")
                                                                            ("Pleurodesis")
                                                                            ("Other" :other)))
                             ("Other surgery" nil
                                              (("Thoracic duct ligation" :date)
                                               ("Nephroctomy" :date)
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
                           (let* ((subgroup20a (lmake-survey-sub-group))
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
                                      as subgroup20b = (lmake-survey-sub-group :order (list q20c))
                                      collect
                                      (progn
                                        ;; Group rules
                                        (when other-p
                                          (let ((q20b-other
                                                 (make-question "Other, please specify" :data-type :string)))
                                            (push q20b-other (group-questions subgroup20b))
                                            (add-rule group20 q20b t subgroup20b ':inline)))
                                        (add-rule subgroup20a q20b t subgroup20b ':inline)
                                        ;; Returns
                                        q20b))))
                             ;; Questions for group
                             (setf (group-questions subgroup20a) qs20b)
                             ;; Group rules
                             (add-rule group20 q20a t subgroup20a ':inline)
                             ;; Returns
                             q20a))))
                ;; Returns
                (setf (group-questions group20) questions20)
                group20))
             (*group21
              (let* ((q21
                      (apply #'lmake-question 20. "Does/did the patient use oxygen at home"
                             :prompt-suffix "?"
                             (choices-options-yes-no)))
                     (group21 (lmake-survey-page-group :order (list q21)))
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
                      (lmake-survey-sub-group :order (list q21a q21b q21c))))
                ;; Group rules
                (add-rule group21 q21 t subgroup21 ':successor)
                ;; Returns
                group21))
             (*group22
              (let* ((q22
                      (apply #'lmake-question 21. "What is the patient's vital status"
                             :prompt-suffix "?"
                             (radio-options
                              (choices-mirror-alist
                               '("Living" "Deceased")))))
                     (group22 (lmake-survey-page-group :order (list q22)))
                     (q22a (make-question "Date of last confirmation" :data-type :date))
                     (subgroup22-living (lmake-survey-sub-group :order (list q22a)))
                     (q22b (make-question "Date of death" :data-type :date))
                     (q22c
                      (apply #'make-question "What was the cause of death"
                             :prompt-suffix "?"
                             (radio-options
                              (choices-breaks-alist
                               '("Respiratory failure" "Infection" "Pulmonary thromboembolism" "Cancer" "Other")))))
                     (subgroup22-dead (lmake-survey-sub-group :order (list q22b q22c)))
                     (q22d (make-question "Please indicate type of infection if known" :data-type :string))
                     (subgroup22-infection (lmake-survey-sub-group :order (list q22d)))
                     (q22e (make-question "Please indicate type of cancer if known" :data-type :string))
                     (subgroup22-cancer (lmake-survey-sub-group :order (list q22e))))
                ;; Group rules
                (add-rule group22 q22 "Living" subgroup22-living ':inline)
                (add-rule group22 q22 "Deceased" subgroup22-dead ':inline)
                (add-rule subgroup22-dead q22c "Infection" subgroup22-infection :inline)
                (add-rule subgroup22-dead q22c "Cancer" subgroup22-cancer :inline)
                ;; Returns
                group22))
             ;;
             ;; Survey
             ;;
             (*survey
              (make-instance 'survey
                             :name "LAM History"
                             :description "This study involves a retrospective medical record review, with a particular focus on pulmonary function tests."
                             :groups (list *group0 *group1 *group8 *group9 *group10 *group15 *group16 *group20 *group21 *group22)
                             :owner *default-study-owner*
                             :published t
                             :priority 1
                             :diary-p nil
                             :ranking-record (make-ranking-record :ranking nil :distribution nil))))
        ;; Returns
        *survey))))
