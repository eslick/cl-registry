(in-package :registry)

;;
;; LAM TV Video Popup
;;

(defun/cc do-lamtv-video ()
  (do-dialog "LAM TV Video: LAMsight Presentation"
    (make-instance 'lamtv-video)))


;;
;; Custom widget for LAM TV plugin
;;

(defwidget lamtv-video (flash)
  ()
  (:default-initargs
      :name "LAM TV Video"
      :src "http://v1.apebble.com/vidplayer.swf"
      :width "480"
      :height "390"
      :id "video_div_1"
      :params '(("flashvars" . "file=lta%2Flta-summit-2008-09-13-ianeslick.flv&streamscript=http%3A%2F%2Fv1.apebble.com%2Fflvstream.php&autostart=true"))))

;;
;; Render frame then base class widget
;;

(defmethod render-widget-body :around ((widget lamtv-video) &rest args)
  (declare (ignore args))
  (with-html
    (:p "Filmed September 13th, 2008 at the TSA/LTA 
         Global Partnership Summit in Brighton, England")
    (call-next-method)
    (render-link (f* (answer widget)) "Return to the Home Page")))

;;     (:object :type "application/x-shockwave-flash" 
;; 	     :data 
;; 	     :width "480"
;; 	     :height "390"
;; 	     :id "video_div_1"
;; 	     (:param :name "flashvars"
;; 		     :value "file=lta%2Flta-summit-2008-09-13-ianeslick.flv&streamscript=http%3A%2F%2Fv1.apebble.com%2Fflvstream.php&autostart=true"))

