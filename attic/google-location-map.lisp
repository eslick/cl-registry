(in-package :registry)

;;
;; Computing patient map datasets for Google Maps
;;

;;
;; Generating datasets
;;


(defun generate-map-dataset (users)
  "Generate a dataset for the visualization map from a set of users"
  (convert-to-google-dataset 
   '(("number" "Lat") ("number" "Lon") ("string" "Info"))
   (let ((rows (nconsolidate-rows (get-geotag-entries users) 2 (cons 0 2))))
     (mapc #'(lambda (row) 
	       (setf (third row) 
		     (format nil "~A patients"
			     (third row))))
			     
	   rows))))

(defun generate-user-intensity-map-dataset (&optional usa)
  (convert-to-google-dataset
   '(("string" "Country")
     ("number" "All Users" "a"))
;;     ("number" "Patients Only" "b"))
   (let ((all-rows (nconsolidate-rows (get-intensity-map-list (all-users) usa)
				      1 '(0 . 1)))
	 (patient-rows (nconsolidate-rows (get-intensity-map-list (all-patients) usa)
					  1 '(0 . 1))))
     (loop for row in all-rows collect
	  (append row 
		  (aif (find (first row) patient-rows :key #'first :test #'equal)
		       (list (second it))
		       (list 0)))))))

(defun get-intensity-map-list (users &optional state-codes-only)
  (with-transaction ()
    (remove-nulls
     (mapcar #'(lambda (user)
		 (awhen (get-geo-country user)
		   (awhen (lookup-iso-country-code it)
		     (cond ((and state-codes-only (equal it "US"))
			    (aif (get-geo-state user)
				 (list (lookup-us-state-code it) user)
				 nil))
			   (state-codes-only nil)
			   (t (list it user))))))
	     users))))

;;
;; Basic patient map
;;

(defwidget patient-google-map ()
  ((map :accessor world-map :initarg :map)))

(defmethod dependencies append ((obj patient-map))
  (list (make-local-dependency :script "discovery/viz")))

(defun make-patient-map ()
  (make-instance 'patient-map
		 :map 
		 (make-instance 'map-visualization
				:dom-id "map1"
				:dataset (generate-map-dataset (all-patients))
				:parameters '((show-tip . "true")))))

(defmethod render-widget-body ((widget patient-map) &rest args)
  (declare (ignore args))
;;  (with-transaction ()
;;    (setf (dataset (world-map widget))
;;	  (generate-map-dataset (all-patients))))
;;  (setf (parameters (intensity-map widget))
;;	`((region . world)
;;	  (width . 330)
;;	  (colors . (,*patient-map-color*))))
  (with-html
    (:h1 (str #!"Users around the World"))
    (render-visualization (world-map widget) :height 250 :width 400)
    (:p)
;;     (:p :style "font-size: small;"
;; 	(str #!"Enlarge: &nbsp;")
;; 	(render-link (f* (do-dialog #!"All Users"
;; 			   (dialog-renderer widget "world" "700")))
;; 		     #!"World")
;; 	(str "&nbsp;")
;; 	(render-link (f* (do-dialog #!"Users in Europe"
;; 			   (dialog-renderer widget "europe" "700")))
;; 		     #!"Europe")
;; 	(str "&nbsp;")
;; 	(render-link (f* (do-dialog #!"Users in the USA"
;; 			   (dialog-renderer widget "usa" "700")))
;; 		     #!"USA")
;; 	(str "&nbsp;")
;; 	(render-link (f* (do-dialog #!"Users South American"
;; 			   (dialog-renderer widget "south_america" "700")))
;; 		     #!"South America")
;; 	(:br)
    (:p (:span :style "font-size: small"
	       (str (format nil "~A/~A users are on this map, ~A ~:*~[users are~;is~:;are~] logged onto the site."
			    (- (length (all-users)) (users-missing-geo-data))
			    (length (all-users))
			    (length (authenticated-server-users)))))
	(:br)
	(unless (get-geo-country (current-user))
	  (htm (:span :style "font-size: small; color: #FF5050;" 
		      (str #!"You are not on this map, please read below!"))))
	(:br)
	(:span :style "font-size: small"
	       (:a :href "/dashboard/discuss/topic/14" "How to read and join this map")))))

;;
;; Intensity map
;;


(defwidget patient-intensity-map (patient-map)
  ())
   
(defparameter *patient-map-color* "#FF6060")
(defvar *intensity-map-data* nil)
(defvar *usa-intensity-map-data* nil)

(defun get-user-intensity-map-dataset (&optional reset)
  (when (or reset (not *intensity-map-data*))
    (setf *intensity-map-data* 
	  (generate-user-intensity-map-dataset)))
  *intensity-map-data*)

(defun get-user-intensity-map-dataset-usa (&optional reset)
  (when (or reset (not *usa-intensity-map-data*))
    (setf *usa-intensity-map-data* 
	  (generate-user-intensity-map-dataset :usa)))
  *usa-intensity-map-data*)

(defun make-patient-intensity-map ()
   (make-instance 'patient-intensity-map
 		 :map 
 		 (make-instance 'intensitymap-visualization
 				:dom-id "intense1"
 				:dataset (get-user-intensity-map-dataset)
 				:parameters nil)))



(defmethod render-widget-body ((widget patient-intensity-map) &rest args)
  (declare (ignore args))
;;  (setf (dataset (world-map widget))
;;	(get-user-intensity-map-dataset))
  (setf (parameters (world-map widget))
	`((region . world)
	  (width . 330)
	  (colors . (,*patient-map-color*))))
  (with-html
    (:h1 (str #!"Users around the World"))
    (render-visualization (world-map widget))
    (:p)
    (:p :style "font-size: small;"
	(str #!"Enlarge: &nbsp;")
	(render-link (f* (do-dialog #!"All Users"
			   (dialog-renderer widget "world" "700")))
		     #!"World")
	(str "&nbsp;")
	(render-link (f* (do-dialog #!"Users in Europe"
			   (dialog-renderer widget "europe" "700")))
		     #!"Europe")
	(str "&nbsp;")
	(render-link (f* (do-dialog #!"Users in the USA"
			   (dialog-renderer widget "usa" "700")))
		     #!"USA")
	(str "&nbsp;")
	(render-link (f* (do-dialog #!"Users South American"
			   (dialog-renderer widget "south_america" "700")))
		     #!"South America")
	(:br)
	(:span :style "font-size: small"
	       (str (format nil "~A/~A users are on this map, ~A ~:*~[users are~;is~:;are~] logged onto the site."
			    (- (length (all-users)) (users-missing-geo-data))
			    (length (all-users))
			    (length (authenticated-server-users)))))
	(:br)
	(unless (get-geo-country (current-user))
	  (htm (:span :style "font-size: small; color: #FF5050;" 
		      (str #!"You are not on this map, please read below!"))))
	(:br)
	(:span :style "font-size: small"
	       (:a :href "/dashboard/discuss/topic/14" "How to read and join this map")))))
	   

(defun dialog-renderer (widget location width)
  (lambda (k)
    (setf (parameters (world-map widget))
	  `((region . ,location)
	    (width . ,width)
	    (colors . (,*patient-map-color*))))
    (let ((old-id (dom-id (world-map widget))))
      (setf (dom-id (world-map widget)) "dialog-temp")
      (when (equal location "usa")
	(setf (dataset (world-map widget))
	      (get-user-intensity-map-dataset-usa)))
      (render-visualization (world-map widget))
      (with-html (:p))
      (render-link (f* (answer k)) "Return to Home Page")
      (setf (dom-id (world-map widget)) old-id)
      (setf (dataset (world-map widget)) 
	    (get-user-intensity-map-dataset)))))


;;
;; Utilities
;;



