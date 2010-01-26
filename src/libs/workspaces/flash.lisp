(in-package :registry)

;; ==============================================
;; Support for embedding Flash applications
;; ==============================================

;; This abstracts all the junk necessary to deploy a flash
;; app inside a weblocks widget.


(defwidget flash-widget ()
  ((name :accessor flash-name :initarg :name)
   (id :accessor flash-id :initarg :id)
   (width :accessor width :initarg :width :initform 400)
   (height :accessor height :initarg :height :initform 300)
   (src :accessor flash-src :initarg :src
	     :initform "/pub/flash/")
   (parameters :accessor flash-parameters :initarg :params :initform nil)
   (file-path :accessor file-path :initarg :file-path
	      :initform nil)
   (deep-linking :accessor deep-linking :initarg :deep-linking 
		 :initform nil)
   (alt-content :accessor alt-content :initarg :alt :initform nil)))

(defmethod dependencies append ((flash flash-widget))
  (list (make-local-dependency :script "swfobject")))

;;    (render-flash-object (flash-name widget) 920 500)))
(defmethod render-widget-body ((widget flash-widget) &rest args)
  (declare (ignore args))
  (send-script 
   (format nil "swfobject.registerObject('~A', '10.0.32.18', '/pub/flash/expressInstall.swf');"
	   (flash-id widget))
   :before-load)
  (with-html
    (:object :id (flash-id widget)
	     :classid "clsid:D27CDB6E-AE6D-11cf-96B8-444553540000" 
	     :width (width widget)
	     :height (height widget)
	     (:param :name "movie" :value (flash-src widget)) ;; flash-src -> "my.swf"
	     (:param :name "quality" :value "high")
	     (:param :name "allowScriptAccess" :value "sameDomain")
	     (render-parameters (flash-parameters widget))
	     (str "<!--[if !IE]>-->")
	     (:object :type "application/x-shockwave-flash" 
		      :data (flash-src widget)
		      :width (width widget)
		      :height (height widget)
		      (render-parameters (flash-parameters widget))
		      (str "<!--<![endif]-->")
		      (insert-alternative-content widget)
		      (str "<!--[if !IE]>-->"))
	     (str "<!--<![endif]-->"))))

(defun insert-alternative-content (flash-widget)
  (with-html
    (:div
     (aif (alt-content flash-widget)
	  (if (functionp it) (funcall it flash-widget)
	      (str it))
	  (htm (:p (:a :href "http://www.adobe.com/go/getflashplayer"
		       (:img :src "http://www.adobe.com/images/shared/download_buttons/get_flash_player.gif" :alt "Get Adobe Flash player"))))))))


;; (defmethod dependencies ((flash flash-widget))
;;   (when (deep-linking flash)
;;     (list (make-flash-dependency 'script flash "deeplinking/deeplinking.js")

;; (defmethod make-flash-dependency (type widget path)
;;   (if (eq type 'script)
;;       (make-instance 'script-dependency 
;; 		     :url (make-flash-url widget path)
;; 		     :local-path (make-flash-path widget path))
;;       (make-instance 'stylesheet-dependency 
;; 		     :url (make-flash-url widget path)
;; 		     :local-path (make-flash-path widget path))))

;; (defun make-flash-url (flash path)
;;   (merge-pathnames 


;;
;; Utility function for rendering a flash <object> tag
;;

(defun render-flash-object (filename width height &key (type "movie") params)
  (when (null params)
    (setf params
	  '(("quality" . "high")
	    ("allowScriptAccess" . "sameDomain"))))
  (with-html
    (:object :width width :height height 
	     (:param :name type :value filename)
	     (render-parameters params)
	     (:embed :src filename 
		     :width width :height height
		     :play "true"
		     :quality "high"
		     :allowScriptAccess "sameDomain"
		     :type "application/x-shockwave-flash"
		     :pluginspage "http://www.adobe.com/go/getflashplayer"))))

(defun render-parameters (params)
  (with-html
    (loop for (name . value) in params do
	 (htm (:param :name name :value value)))))

			    