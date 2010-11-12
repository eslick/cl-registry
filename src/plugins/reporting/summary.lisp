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
  ((current :accessor current :initarg :current :initform (first *canned-questions*))))

(defmethod dependencies append ((obj simple-report))
  `(,(make-local-dependency :script "discovery/viz")
    ,(make-instance 'script-dependency :url 
		    (get-site-config-param :google-api-key))))

;;
;; Datasets
;;

(defun get-patient-distribution-dataset ()
  (json:encode-json-to-string 
   (pre-process-google-dataset
    (convert-to-google-dataset
     '(("string" "Center") ("number" "# Patients"))
     (mapcar (lambda (center)
	       (list (short-name center) (patient-count center)))
	     (get-instances-by-class 'center))))))

(defun test-dataset ()		  
  "[[['string', 'Center'], 'MIT', 'Mayo', 'Fred'],
    [['number', 'Patients'], 10, 40, 9]]")

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

(defmethod render-widget-body ((widget simple-report) &rest args)
  (declare (ignore args))
  (let ((width 900)
	(height 500))
    (with-html
      (:h1 "Patients in the Study")
      (:div :class "visualization"
	    :id "viz1"
	    :style (viz-style width height)))
    (draw-visualization "viz1" "column-chart-visualization" 
			(get-patient-distribution-dataset)
			(render-chart-params 
			 `((width . ,width) (height . ,height))))))

