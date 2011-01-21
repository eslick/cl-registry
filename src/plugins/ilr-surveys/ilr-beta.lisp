;; -*- Mode:Lisp; tab-width:2; indent-tabs-mode:nil -*-

(in-package :registry)

;; Create users and centers

(defun create-ilr-beta-users ()
  (let ((center nil)
        (count-centers 0.)
        (user nil)
        (count-users 0.)
        (clinician nil)
        (count-clinicians 0.))
    (flet ((mk-center (num short-name name location)
             (declare (ignore num location))
             (setq center
                   (acond
                     ((get-center short-name t)
                      (format t "~&Warning: Center ~A already exists" short-name)
                      it)
                     ((make-center short-name name)
                      (incf count-centers)
                      it))))
           (mk-user (center-num name first middle last &optional titles email)
             (declare (ignore center-num middle titles))
             (setq user
                   (acond
                     ((get-user name)
                      (format t "~&Warning: User ~A already exists" name)
                      it)
                     ((user-add name name :first first :last last :email email)
                      (incf count-users)
                      it)))
             (setq clinician
                   (acond
                     ((get-clinician user center)
                      (format t "~&Warning: Clinician for user ~A center ~A already exists"
                              name (short-name center))
                      it)
                     ((make-clinician user center)
                      (incf count-clinicians)
                      it)))
             ;; Returns
             user))
      (with-transaction ()
        (mk-center 0 "Clozure" "Clozure Associates LLC" "Somerville, Massachusetts")
        (mk-user 0 "kmcorbett" "Keith" "M." "Corbett" "" "kmcorbett@clozure.com")
        (mk-center 1 "BWH" "Divisions of Surgical Critical Care, Cardiac and Thoracic Anesthesia, Brigham and Women's Hospital, Harvard Medical School" "Boston, Massachusetts")
        (mk-user 1 "mnurok" "Michael" "" "Nurok" "M.D., Ph.D." "mnurok@partners.org")
        (mk-user 1 "sbillmeier" "Sarah" "" "Billmeier" "M.D." "sbillmeier@partners.org")
        (mk-center 2 "MIT-Media-Lab" "MIT Media Laboratory, Massachusetts Institute of Technology" "Cambridge, Massachusetts")
        (mk-user 2 "ieslick" "Ian" "" "Eslick" "M.S., M.Eng." "eslick@media.mit.edu")
        (mk-user 2 "fmoss" "Frank" "" "Moss" "Ph.D." "fmoss@media.mit.edu")
        (mk-center 3 "InCor" "Pulmonary Division, Heart Institute (InCor), University of Sao Paulo" "Sao Paulo, Brazil")
        (mk-user 3 "carvalho" "Carlos" "R. R." "Carvalho" "M.D., Ph.D." "crrc.cedot@terra.com.br")
        (mk-center 4 "Ruhrlandklinik" "Department of Pneumology/Allergy, Ruhrlandklinik–UniversitätsklinikuEsse" "Germayn")
        (mk-user 4 "ucostabel" "Ulrich" "" "Costabel" "M.D., Ph.D." "ulrich.costabel@ruhrlandklinik.uk-essen.de")
        (mk-center 5 "Columbia" "Division of Pulmonary and Critical Care, Department of Medicine, College of Physicians and Surgeons of Columbia University" "New York, New York")
        (mk-user 5 "jdarmiento" "Jeanine" "" "D’Armiento" "M.D., Ph.D." "jmd12@columbia.edu")
        (mk-center 6 "St.Vincents" "Lung Transplantation Group, Division of Thoracic Medicine, St. Vincent’s Hospital" "Darlinghurst, NSW, Australia")
        (mk-user 6 "aglanville" "Allan" "R." "Glanville" "M.D., F.R.A.C.P." "aglanville@stvincents.com.au")
        (mk-center 7 "OSG" "Unita` di Pneumologia e Terapia Semi-Intensiva Respiratoria, Ospedale San Giuseppe" "Milan, Italy")
        (mk-user 7 "sharari" "Sergio" "" "Harari" "M.D." "sharari@ilpolmone.it")
        (mk-center 8 "BWH-DF" "Center for LAM Research and Clinical Care, Division of Pulmonary and Critical Care Medicine, Brigham and Women’s Hospital, and Dana Farber Cancer Institute" "Boston, Massachusetts")
        (mk-user 8 "ehenske" "Elizabeth" "P." "Henske" "M.D." "ehenske@partners.org")
        (mk-center 9 "NHO-KC" "Department of Diffuse Lung Diseases and Respiratory Failure, Clinical Research Center, National Hospital Organization Kinki-Chuo Chest Medical Center" "Osaka, Japan")
        (mk-user 9 "yinoue" "Yoshikazu" "" "Inoue" "M.D., Ph.D." "giichi@kch.hosp.go.jp")
        (mk-center 10 "U.Nottingham" "Division of Therapeutics and Molecular Medicine and Nottingham Respiratory Biomedical Research Unit, University of Nottingham" "Nottingham, UK")
        (mk-user 10 "sjohnson" "Simon" "R." "Johnson" "D.M., F.R.C.P." "Simon.Johnson@nottingham.ac.uk")
        (mk-center 11 "H.Cochin" "Service de Pneumologie, Hopital Cochin" "Paris, France")
        (mk-user 11 "jlacronique" "Jacques" "" "Lacronique" "M.D." "jacques.lacronique@cch.aphp.fr")
        (mk-center 12 "U.Vaudois" "Department of Respiratory Medicine, Clinic for Interstitial and Rare Lung Diseases, Centre Hospitalier Universitaire Vaudois" "Lausanne, Switzerland")
        (mk-user 12 "rlazor" "Romain" "" "Lazor" "M.D." "romain.lazor@chuv.ch")
        (mk-center 13 "NHLBI" "Translational Medicine Branch, National Heart, Lung and Blood Institute, National Institutes of Health" "Bethesda, Maryland")
        (mk-user 13 "jmoss" "Joel" "" "Moss" "M.D." "mossj@nhlbi.nih.gov")
        (mk-center 14 "Stanford" "Division of Pulmonary and Critical Care Medicine, Stanford University Medical Center" "Stanford, California")
        (mk-user 14 "sruoss" "Stephen" "J." "Ruoss" "M.D." "ruoss@stanford.edu")
        (mk-center 15 "Mayo" "Division of Pulmonary and Critical Care Medicine, Mayo Clinic" "Rochester, Minnesota")
        (mk-user 15 "jryu" "Jay" "H." "Ryu" "M.D." "ryu.jay@mayo.edu")
        (mk-center 16 "Juntendo" "Department of Respiratory Medicine, Juntendo University School of Medicine" "Tokyo, Japan")
        (mk-user 16 "kseyama" "Kuniaki" "" "Seyama" "M.D., Ph.D." "kseyama@med.juntendo.ac.jp")
        (mk-center 17 "ZPT" "Pneumologisches Forschungsinstitut GmbH am Krankenhaus Großhansdorf, Zentrum fur Pneumologie und Thoraxchirurg" "Großhansdorf, Germany")
        (mk-user 17 "hwatz" "Henrik" "" "Watz" "M.D., Ph.D." "h.watz@pulmoresearch.de")
        (mk-center 18 "PekingUnion" "Department of Respiratory Medicine, Peking Union Medical College Hospital, Chinese Academy of Medical Sciences" "Beijing, China")
        (mk-user 18 "kfxu" "Kai–Feng" "" "Xu" "M.D." "kaifeng.xu@gmail.com")
        (mk-center 19 "Partners" "Partners Human Research Committees, Partners Healthcare" "Boston, Massachusetts")
        (mk-user 19 "ehohmann" "Elizabeth" "L." "Hohmann" "M.D." "ehohmann@partners.org")
        (mk-center 20 "LTA" "LAM Treatment Alliance" "Cambridge, Massachusetts")
        (mk-user 20 "afarber" "Amy" "" "Farber" "Ph.D" "amy_farber@hms.harvard.edu")
        (mk-user 20 "rabrusci" "Richard" "" "Abrusci" "" "rabrusci@lamtreatmentalliance.org")
        (mk-user 20 "aschiermeier" "Andrew" "" "Schiermier" "" "aschiermeier@lamtreatmentalliance.org")
        (mk-user 20 "bgerety" "Bob" "" "Gerety" "" "bgerety@lamtreatmentalliance.org")
        (mk-user 20 "dhao" "Daphne" "" "Hao" "" "dhao@lamtreatmentalliance.org")
        (mk-user 20 "ahickey" "Anthony" "" "Hicky" "" "ahickey@lamtreatmentalliance.org")
        (mk-user 20 "mkabadi" "Mohan" "" "Kabadi" "" "mkabadi@lamtreatmentalliance.org")
        (mk-user 20 "bpulliam" "Brian" "" "Pulliam" "" "bpulliam@lamtreatmentalliance.org")
        )
      ;; Returns
      (values count-users count-centers))))

(defun create-ilr-beta-discuss (&key (owner (current-user t)) force &aux cat)
  (with-transaction ()
    ;; Delete old topics and categories
    (when force
      (mapcar #'drop-instance (get-instances-by-class 'forum-topic))
      (mapcar #'drop-instance (get-instances-by-class 'forum-category)))
    ;; Create new categories and topics
    (setq cat (make-instance 'forum-category :name "Feature Requests"))
    (setq cat (make-instance 'forum-category :name "General"))
    (setq cat (make-instance 'forum-category :name "Using ILR"))
    (let ((topic-content
           "<h2>Help for new users of the International LAM Registry </h2>
<p>Click on the <strong>Home tab</strong> to manage patient record information. 
Go down to Add / Edit Patient. Click on \"Add\" and add a patient record.
ILR will create a *unique internal ID* for each patient.</p>
<p>Optionally: Record an <em>external ID</em> for the patient, for example your own electronic record ID. </p>
<p>Click on the <strong>Collect tab</strong> to see one or more clinical studies running on the ILR. 
From this page you can fill in survey information for the selected patient.</p>
<p>When you have created multiple patient records, you can select a patient ID 
from the drop-down menu on the Home tab or on the Collect tab. 
You can select a patient and return to the Collect tab to modify or add survey information for the patient. </p>
<p>The journal article about the ILR is available under the <strong>Docs tab</strong>. 
Click on ILR then click on the article to open.</p>
<p>Need more <strong>help</strong>? 
Send email to  <a href=\"mailto:admin@internationallamregistry.org\">admin@internationallamregistry.org</a></p>"))
      ;; Create forum topic
      (make-instance 'forum-topic
                     :number (next-topic-number) :category cat :subject "Welcome to ILR!"
                     :topic-tags '()
                     :owner owner
                     :content-type ':html
                     :content topic-content)
      ;; Alternatively, create an article to use as a template, later create forum topic by hand
      ;; Inspired by bug #286 and other problems observed creating forum topic programmatically
      #+NIL
      (make-instance 'article :title "Welcome to ILR!"
                     :owner owner :page "welcome-to-ilr" :order 1
                     :content-type ':html
                     :content topic-content))))

(defun create-ilr-beta ()
  (create-ilr-beta-users)
  (create-ilr-beta-discuss))

(defun print-ilr-beta-clinicians (&optional (out *standard-output*))
  (loop for center in (get-instances-by-class 'center) do
       (progn
         (format out "~%Center: ~A" (center-name center))
         (loop for clinician in (get-clinicians-for-center center) do
              (let ((user (user clinician)))
                (format out "~%         ~A~@[  (center admin)~]"
                        (if (null user)
                            "anonymous"
                            (format nil "~A - ~@[~A, ~]~@[~A ~]~@[~A ~]"
                                    (username user)
                                    (last-name user)
                                    (first-name user)
                                    (user-email user)))
                        (center-admin-p clinician)))))))

                          
