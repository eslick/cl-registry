(in-package :registry)

;;
;; User controls
;;

(defun make-user-controls ()
  (make-instance 'user-controls))

(defwidget user-controls (widget)
  ())

(defmethod render-widget-body ((widget user-controls) &rest args)
  (declare (ignore args))
  (render-user-control-box (home-page-map (widget-parent widget))))

(defun render-user-control-box (map-widget)
  (with-html
    (:div :class "user-controls-box"
	  (:h1 (str #!"User Options"))
	  (:ul
	   (:li (render-link (f* (do-preferences map-widget))
			     #!"User Preferences"))
           (:li (with-html
                  (:a :href "/about"
                      (str #!"Help"))))
	   (:li (render-link (lambda (&rest args)
			       (declare (ignore args))
			       (do-change-password-dialog))
			     #!"Change Password"))
	   (:li (:a :href "http://www.clinicaltrials.gov/ct2/results?term=Lymphangioleiomyomatosis" "View Clinical Trials") "<span>(at clinicaltrials.gov)</span>")))))

(defun/cc do-preferences (map-widget &optional tab)
  (let ((user (current-user)))
    (do-dialog #!"User Preferences" ;; (widget-parent widget)
	       (make-preferences-page tab))
    (when (and (get-preference 'map-me-p user)
	       (get-geo-country user)
	       (not (get-geotag user)))
      (make-geotag user)
      (when (get-geotag user)
	(setf (map-dataset map-widget)
	      (generate-user-map-dataset (mappable-users)))
	(post-action-redirect (request-uri-path))))))
