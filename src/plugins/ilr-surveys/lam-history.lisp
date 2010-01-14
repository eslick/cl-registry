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

(defun create-lam-history (&key (owner (current-user)))
  (with-transaction ()
    (let* ((*default-study-owner* owner)
           (group-counter 0.)
           (*group0
            (make-instance 'survey-group
                           :name (format nil "LAM History Part ~D - Clinician Header" (incf group-counter))
                           :order nil
                           :advice "Thank you for your involvement in this research study and your collaboration with the International LAM Registry!
<P>We will use this information to examine whether women diagnosed with LAM under age 25 have a more rapid pulmonary function decline relative to those over the age of 55.
<P>This study involves a retrospective medical record review, with a particular focus on pulmonary function tests.  
In completing the following form, we ask that you comply with your local laws and regulations regarding access and use of patient records, and obtain consent for record use whenever indicated.
Data identifiers such as name and date of birth are used to eliminate duplicate records for patients receiving care at multiple institutions, however records will be given a unique identifier and stored in de-identified form.  
For further information about data use please see our
<A HREF=\"/forms/ilr-data-use\">International LAM Registry data use form</A>.
<P>For this study, please complete the following data entry form for current and former LAM patients in your care who were:
<UL>
<LI>diagnosed with LAM at age 25 or under
<LI>diagnosed with LAM at age 55 or older
</UL>
<P>The time it takes to complete the following data entry form will vary, depending on the type of medical records you are using and the number of pulmonary function tests to enter.  
You may save your work at any point to complete at a later time.
<P> Additionally, please encourage all of your LAM patients to register on LAMsight.org!"
                           :owner *default-study-owner*))
           (q1
            (make-question "Patient name"
                           :prompt "1. Patient name:"
                           :data-type :string))
           (q2
            (make-question "Patient date of birth"
                           :prompt "2. Patient date of birth:"
                           :data-type :date))
           (q3
            (make-question "Country where patient receives medical care"
                           :prompt "3. Country where patient receives medical care:"
                           ;; !! TODO: Country names from database !!
                           :data-type :string))
           (q4
            (make-question "Date of diagnosis of LAM or TSC-LAM"
                           :prompt "4. Date of diagnosis of LAM or TSC-LAM:"
                           :data-type :date))
           (q5
            (apply #'make-question "Diagnosis type"
                   :prompt "5. Diagnosis type:"
                   (radio-options
                    '(("Sporadic LAM" . "LAM")
                      ("Tuberous Sclerosis Complex and LAM (TSC-LAM)" . "TSC-LAM")
                      ("Unknown" . "Unknown")))))
           ;; Rule: if q5 answer is TSC-LAM...
           (q6
            (make-question "If the patient has TSC-LAM, what symptoms does the patient have"
                           :prompt "6. If the patient has TSC-LAM, what symptoms does the patient have?
<P>Please check all that apply:"
                           :data-type :multichoice
                           ;;:view-type :selection
                           :choices
                           (choices-mirror-alist
                            '("None" "Developmental delay" "Behavioral problem" "Other" "Unknown"))))
           ;; Rule: if q6 answer included Other...
           (q6o
            (make-question "Other TSC-LAM symptoms" :data-type :string :view-type :text-field))
           ;; Rule: if q5 answer is TSC-LAM...
           (q7
            (make-question "If the patient has TSC-LAM, what organ systems are affected by tumors"
                           :prompt "7. If the patient has TSC-LAM, what organ systems are affected by tumors?
<P>Please check all that apply:"
                           :data-type :multichoice
                           ;;:view-type :selection
                           :choices
                           (choices-mirror-alist
                            '("Kidneys" "Heart" "Eyes" "Skin" "Lungs" "Other" "Unknown"))))
           ;; Rule: if previous answer included Other...
           (q7o
	    (make-question "Other TSC-LAM symptoms" :data-type :string :view-type :text-field))
           (*questions1 (list q1 q2 q3 q4 q5))
           (*group1
            (let ((group1
                   (make-instance 'survey-group
                                  :name (format nil "LAM History Part ~D" (incf group-counter))
                                  :order *questions1
                                  :owner *default-study-owner*))
                  (subgroup1
                   (make-instance 'survey-group
                                  :order (list q6 q7)
                                  :owner *default-study-owner*))
                  (subgroup2
                   (make-instance 'survey-group
                                  :order (list q6o)
                                  :owner *default-study-owner*))
                  (subgroup3
                   (make-instance 'survey-group
                                  :order (list q7o)
                                  :owner *default-study-owner*)))
              (add-rule group1 q5 "TSC-LAM" subgroup1 :successor)
              (add-rule group1 q6 "Other" subgroup2 :successor)
              (add-rule group1 q7 "Other" subgroup3 :successor)
	      ;; Returns
              group1))
           ;; How was LAM diagnosed?
           (q8
	    (make-question "How was LAM diagnosed"
			   :prompt "8. How was LAM diagnosed?"
			   :data-type :multichoice
			   :choices
			   (choices-mirror-alist
			    '("Pathological diagnosis"
			      "Clinical diagnosis without biopsy"
			      "Other"
			      "Unknown"))))
           ;; Rule: if q8 was pathological diagnosis
           (q8a
            (make-question "If pathological diagnosis, please indicate the type of biopsy (check all that apply)"
                           :data-type :multichoice
                           :choices
                           (choices-mirror-alist
                            '("Lung biopsy" "Biopsy of lymph node" "Biopsy of other mass"))))
           (q8a2
            (apply #'make-question "If lung biopsy, please specify type"
                   (radio-options
                    '(("Open lung biopsy" . "open")
                      ("Video-assisted thorascopic surgery (VATS) biopsy" . "VATS")
                      ("Transbronchial lung biopsy" . "Transbronchial")))))
           (q8b
            (make-question "If clinical diagnosis, what was used to make the diagnosis? Please check all that apply:"
                           :data-type :multichoice
                           :choices
                           (choices-mirror-alist
                            '("Chest CT" "Other imaging findings" "Cliniical picture" "Pulmonary function tests" "Other"))))
           ;; Rule: if q8b is "Other imaging findings"
           (q8bo1
            (make-question "Other imaging findings. Please specify"
                           :data-type :string))
           (q8bo2
            (make-question "Other:" :data-type :string))
           (*questions8 (list q8))
           (*group8
            (let ((group8
                   (make-instance 'survey-group
                                  :name (format nil "LAM History Part ~D" (incf group-counter))
                                  :order *questions8
                                  :owner *default-study-owner*))
                  (subgroup8a
                   (make-instance 'survey-group
                                  :order (list q8a)
                                  :owner *default-study-owner*))
                  (subgroup8a2
                   (make-instance 'survey-group
                                  :order (list q8a2)
                                  :owner *default-study-owner*))
                  (subgroup8b
                   (make-instance 'survey-group
                                  :order (list q8b)
                                  :owner *default-study-owner*))
                  (subgroup8bo1
                   (make-instance 'survey-group
                                  :order (list q8bo1)
                                  :owner *default-study-owner*))
                  (subgroup8bo2
                   (make-instance 'survey-group
                                  :order (list q8bo2)
                                  :owner *default-study-owner*))
)
              (add-rule group8 q8 "Pathological diagnosis" subgroup8a :inline)
              (add-rule group8 q8a "Lung biopsy" subgroup8a2 :inline)
              (add-rule group8 q8 "Clinical diagnosis without biopsy" subgroup8b :inline)
              (add-rule group8 q8b "Other imaging findings" subgroup8bo1 :inline)
              (add-rule group8 q8b "Other" subgroup8bo2 :inline)
              (add-rule group8 q8b "Other imaging findings" subgroup8bo1 :inline)
              ;; Returns
              group8))
	   ;; Q9 outline
           ;; ?? TODO: Should q9 be related to q8 by rule ??
           (*group9
	    (let* ((group9
		    (make-instance 'survey-group
				   :name (format nil "LAM History Part ~D" (incf group-counter))
				   :advice "9.If a biopsy was performed, what were the histopathological findings?"
				   :owner *default-study-owner*))
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
				       (radio-options (choices-mirror-alist (mapcar #'first options))))))
			   (loop for option in options
				as value = (first option)
				as choices = (rest option)
				when choices
				do
				(let* ((question2
					(apply #'make-question (format nil "~A ~A" name (gensym))
					       :prompt nil
					       (radio-options (choices-mirror-alist choices))))
				       (subgroup
					(make-instance 'survey-group
						       :name (gensym)
						       :order (list question2)
						       :owner *default-study-owner*)))
				  (add-rule group9 question value subgroup :inline)))
			   ;; Return 
			   question))))
	      (setf (group-questions group9) questions)
	      ;; Returns
	      group9))
	   ;; Q10 simple multichoice
           (q10
            (make-question "How did the patient originally present"
                           :prompt "10. How did the patient originally present?
What symptom, finding or event led to the eventual diagnosis of LAM?"
                           :data-type :multichoice
                           ;;:view-type :selection
                           :choices
                           (choices-mirror-alist
                            '("Exertional dyspnea"
                              "Spontaneous pneumothorax"
                              "Pleural effusion"
                              "Chylous ascites"
                              "Other"))))
           ;; Rule: if q10 included Other...
           (q10o
            (make-question "Other symptom(s) originally presented"
                           :data-type :string))
           (q11
            (make-question "How old was the patient at time of the first symptoms attributed to LAM"
                           :prompt "11. How old was the patient at time of the first symptoms attributed to LAM?"
                           :data-type :number))
	   (q12
	    (apply #'make-question "What is the patient's smoking history"
		   :prmopt "12. What is the patient's smoking history?"
		   (radio-options
		    (choices-mirror-alist
		     '("Current smoker" "Former smoker" "Never a smoker" "Unknown")))))
	   (q13
	    (apply #'make-question "Has the patient ever had a pneumothorax"
		   :prompt "13. Has the patient ever had a pneumotharax?"
		   (radio-options
		    (choices-mirror-alist
		     '("Yes" "No" "Unknown")))))
	   (q14
	    (apply #'make-question "Has the patient ever had a pleural effusion"
		   :prompt "14. Has the patient ever had a pleural effusion?"
		   (radio-options
		    (choices-mirror-alist
		     '("Yes" "No" "Unknown")))))
	   (q15
	    (apply #'make-question "What extrapulmonary lesions does the patient have"
		   :prompt "What extrapulmonary lesions does the patient have?
<P>Please check all that apply:"
		   (multi-choices-options
		    (choices-mirror-alist
		     '("None" "Renal angiomyolipoma" "Non-renal angiomyolipoma" "Lymphangiomyoma"
		       "Axillary lymph node swelling" "Chylous ascites" "Chylous pleural effusion"
		       "Other")))))
	   (q16
	    (make-question "What was the patient's age of menarche"
			   :prompt "16. What was the patient's age of menarche?"
			   :data-type :number)) ;?? TODO: "Unknown" checkbox
	   (q17
	    (apply #'make-question "Did the patient take oral contraceptive pills before diagnosed with LAM"
		   :prompt "17. Did the patient take oral contraceptive pills before diagnosed with LAM?"
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
	    (apply #'make-question "Has the patient ever been pregnant"
		   :prompt "18. Has the patient ever been pregnant?"
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
	    (apply #'make-question "Has the patient gone through menopause"
		   :prompt "18. Has the patient gone through menopause?"
		   (radio-options
		    (choices-mirror-alist
		     '("Yes" "No" "Unknown")))))
           (*questions10 (list q10 q11 q12 q13 q14 q15 q16 q17 q18 q19))
           (*group10
	    (let ((group10
             (make-instance 'survey-group
                            :name (format nil "LAM History Part ~D" (incf group-counter))
                            :order *questions10
                            :owner *default-study-owner*))
            (subgroup10o
                   (make-instance 'survey-group
                                  :order (list q10o)
                                  :owner *default-study-owner*)))
        (add-rule group10 q10 "Other" subgroup10o :inline)
        (add-rule group10 q18 "Yes" q18-table :inline)
        (add-rule group10 q17 "Yes" q17b :inline)
	      ;; Returns
	      group10))
           ;;
           ;; Survey
           ;;
           (*survey
            (make-instance 'survey
                           :name "LAM History"
                           :description "This study involves a retrospective medical record review, with a particular focus on pulmonary function tests."
                           :groups (list *group0 *group1 *group8 *group9 *group10)
                           :owner *default-study-owner*
                           :published t
                           :priority 1
                           :diary-p nil
                           :ranking-record (make-ranking-record :ranking nil :distribution nil))))
      ;; Returns
      *survey)))

                 
