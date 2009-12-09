(in-package :registry)

;;
;; Virtualearth-based user location map
;;


;;
;; Computing a dataset from a set of users
;;

(defun generate-user-map-dataset (users)
  (nconsolidate-rows (get-geotag-entries users) 2 (cons 0 2)))

(defun users-missing-geo-data ()
  (- (length (all-users))
     (length (select-if 
	      (lambda (geotag)
		(and (geotag-user geotag)
		     (get-preference :map-me-p (geotag-user geotag))))
	      (get-instances-by-class 'geotag)))))

;;
;; User map pop-up dialog
;;

(defwidget user-map-widget ()
  ((dataset :accessor map-dataset :initarg :dataset :initform nil)))

(defmethod dependencies append ((obj user-map-widget))
  (list (make-instance 'script-dependency 
		       :url "https://dev.virtualearth.net/mapcontrol/mapcontrol.ashx?v=6.2&mkt=en-us&s=1")
	(make-local-dependency :script "discovery/vemap")))

(defmethod render-widget-body ((map user-map-widget) &rest initargs)
  (declare (ignore initargs))
  (with-html
    (:h1 (str #!"Where are our users?"))
    (:div :id "home-map" :class "home-map")
    (:p 
     	(htm (:span :style "font-size: 100%;"
		    (render-link 
		     (f* (do-dialog #!"Where are our users?"
				    (make-user-map-dialog map)))
		     #!"Explore this map")))
	(:br)
	(:span :style "font-size: 100%;"
	       (str (format nil "~A of ~A users are shown here (~A ~:*~[users are~;is~:;are~] logged in)"
			    (- (length (all-users)) (users-missing-geo-data))
			    (length (all-users))
			    (length (authenticated-server-users)))))
	(:br)
	(unless (and (get-geotag (current-user))
		     (get-preference :map-me-p (current-user)))
	  (htm (:span :style "font-size: small; color: #FF5050;" 
		      (str #!"(You are not on this map, ")
		      (render-link (f* (do-preferences map "personal"))
				   "enter your address and be seen!")
		      ")")))
	(when (is-admin-p) 
	  (htm (:span :style "font-size: small"
		      (render-link 
		       (f* (do-dialog "LAM TV: LAMsight Introduction"
			     (make-instance 'lamtv-video)))
		       #!"LAMTV"))))))
  (send-script (compute-ve-map-script map)))

(defun compute-ve-map-script (user-map-widget &optional dset)
  (let ((dataset (or dset (map-dataset user-map-widget))))
    (ps::ps* 
     `(progn 
      	(defun setup-vemap (map)
	  (let ((layer (new (*v-e-shape-layer))))
	    ,@(mapcar (lambda (point) (ve-add-custom-pin point)) dataset)
	    ((@ layer *set-clustering-configuration) 
	     (@ *v-e-clustering-type *grid))
	    ((@ map *add-shape-layer) layer)))))))
  
  
(defun ve-add-custom-pin (data-point)
  (dbind (lat long count) data-point
    `(progn
       (setf pin (new (*v-e-shape (@ *v-e-shape-type *pushpin)
				  (new (*v-e-lat-long ,lat ,long)))))
       ((@ pin *set-custom-icon)
			  ,(format nil "<div class='ve-pin-style'>~A<div class='text'></div></div>"
				   count))
       ((@ pin *set-title) ,(format nil "~A ~A" count (if (= count 1)
						       #!"patient"
						       #!"patients")))
       ((@ layer *add-shape) pin))))
				  

;;
;; User map pop-up dialog
;;

(defwidget user-map-dialog (user-map-widget)
  ())

(defun make-user-map-dialog (map)
  (make-instance 'user-map-dialog
		 :dataset (map-dataset map)))

(defmethod render-widget-body ((map user-map-dialog) &rest initargs)
  (declare (ignore initargs))
  (with-html
    (:div :id "home-map-dialog" :class "home-map-dialog")
    (:p (render-link (f* (answer map)) "Return to the Home Page")))
  (send-script (compute-ve-map-script map))
  (send-script (ps::ps* `(setup-vemaps))))

