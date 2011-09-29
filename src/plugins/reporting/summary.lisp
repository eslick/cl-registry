(in-package :registry)

(registry-proclamations)

;;
;; Canned report listings
;;

(defparameter *canned-questions*
  '("Date of birth"))

;;
;; 
;;

;;
;; Dispatcher
;;

(defwidget registry-pane () ())

(defwidget simple-report (registry-pane)
  ((current :accessor current :initarg :current :initform :by-center)))

(defmethod dependencies append ((obj simple-report))
  `(,(make-local-dependency :script "discovery/viz")
    ,(make-instance 'script-dependency :url 
		    (get-site-config-param :google-api-key))))

;;
;; Datasets
;;

(defun test-center-p (center)
  (member (short-name center) 
	  '("LTA" "lamhtest" "MIT-Media-Lab" "Clozure")
	  :test 'equal))

;; Patient totals

(defun get-patient-distribution-dataset ()
  (json:encode-json-to-string 
   (pre-process-google-dataset
    (convert-to-google-dataset
     '(("string" "Center") ("number" "# Patients"))
     (mapcar (lambda (center)
	       (list (short-name center) (patient-count center)))
	     (loop for center in (get-instances-by-class 'center)
		  unless (test-center-p center)
		  collect center))))))


;; Age

(defvar *age-question* nil)

(defun age-question (patient)
  (unless *age-question*
    (setf *age-question* (get-question "Date of Pulmonary Function Test")))
  *age-question*)

(defun age-at-diagnosis (patient)
  (awhen (get-answer (age-question) patient)
    (value it)))

(defun get-diagnosis-age-dataset ()
  (json:encode-json-to-string
   (pre-process-google-dataset
    (convert-to-google-dataset
     '(("string" "Age") ("number" "# Patients"))
     (compute-histogram
      (collect #'age-at-diagnosis (get-instances-by-class 'patient)))))))

(defun test-dataset ()		  
  "[[['string', 'Center'], 'MIT', 'Mayo', 'Fred'],
    [['number', 'Patients'], 10, 40, 9]]")

;; PFT

(defvar *pft-question* nil)

(defun pft-question ()
  (unless *pft-question* 
    (setf *pft-question* (get-question "Date of Pulmonary Function Test")))
  *pft-question*)

(defun pft-results (patient)
  (get-user-answers (pft-question) patient))

(defun pft-qualifying-patient (patient)
  (let ((age (age-at-diagnosis patient))
	(pfts (pft-results patient)))
    (and age (or (<= age 25) (>= age 55))
	 (>= (length pfts) 2))))

(defun pft-qualifying-patients ()
  (loop for patient in (get-instances-by-class 'patient)
     when (pft-qualifying-patient patient)
     collect patient))

(defun counts-per-center (patients)
  (let ((hash (make-hash-table :test #'equal)))
    (loop for patient in patients
       for center = (center patient)
       for cname = (short-name center) 
       unless (test-center-p center)
       do (if (null (gethash cname hash))
	      (setf (gethash cname hash) 1)
	      (incf (gethash cname hash))))
    (group (flatten (hash-items hash)) 2)))

(defun pft-qualifying-patients-dataset ()
  (json:encode-json-to-string 
   (pre-process-google-dataset
    (convert-to-google-dataset
     '(("string" "Center") ("number" "# Patients"))
     (counts-per-center
      (pft-qualifying-patients))))))

;;
;; Rendering aids
;;

(defun draw-visualization (id type dataset params)
  (send-script
   (ps:ps* `((@ *event on-ready)
	     (lambda ()
	       (set-timeout
		(lambda ()
		  (draw-visualization ,id ,type
				      ,dataset
				      ,params))))))))
  
(defun viz-style (width height)
  (format nil "width: ~A; height: ~A" width height))

(defun render-chart-params (alist)
  (json:encode-json-alist-to-string alist))

;;
;; Actual simple report body
;; 

(defun render-simple-report-header (widget)
  (with-html
    (:div
     (render-link (f* (setf (current widget) :by-center))
		  "Patients by Center")
     "&nbsp; | &nbsp;"
     (render-link (f* (setf (current widget) :qualifying))
		  "Qualifying Patients")
     "&nbsp; | &nbsp;"
     (render-link (f* (setf (current widget) :age-dist))
		  "Age at Diagnosis")
     (:hr))))

(defun render-viz/patients-by-center (width height)
  (with-html 
    (:h1 "Total Patients by Center")
    (:div :class "visualization"
	  :id "viz1"
	  :style (viz-style width height)))
  (draw-visualization "viz1" "column-chart-visualization" 
		      (get-patient-distribution-dataset)
		      (render-chart-params 
		       `((width . ,width) (height . ,height)))))

(defun render-viz/pft-qualifying (w h)
  (with-html
    (:h1 "Qualifying Patients")
    (:div :class "visualization"
	  :id "viz2"
	  :style (viz-style w h)))
  (draw-visualization "viz2" "column-chart-visualization" 
		      (pft-qualifying-patients-dataset)
		      (render-chart-params 
		       `((width . ,w) (height . ,h)))))

(defun render-viz/age-dist (w h)
  (with-html
    (:h1 "Age at Diagnosis")
    (:div :class "visualization"
	  :id "viz3"
	  :style (viz-style w h)))
  (draw-visualization "viz3" "column-chart-visualization" 
		      (get-diagnosis-age-dataset)
		      (render-chart-params 
		       `((width . ,w) (height . ,h)))))

(defun render-viz/empty (width height)
  (with-html
    (:h1 "No report found")))

(defmethod render-widget-body ((widget simple-report) &rest args)
  (declare (ignore args))
  (let ((width 900)
	(height 500))
    (render-simple-report-header widget)
    (case (current widget)
      (:by-center (render-viz/patients-by-center width height))
      (:qualifying (render-viz/pft-qualifying width height))
      (:age-dist (render-viz/age-dist width height))
      (t (render-viz/empty)))))

