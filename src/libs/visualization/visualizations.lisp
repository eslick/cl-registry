(in-package :registry)

(registry-proclamations)



;; =================================================
;;  Generic presentation layer for visualizations
;; =================================================

;; questions, groups, workflow?
;; visualization object is a particular set of params, etc.
;; Generic way of getting datasets from objects
;; Generic way of initializing a presentation with that data

(defgeneric render-visualization (viz &key dataset &allow-other-keys)
  (:documentation "Take an object and render a view of that object"))

;; =================================================
;;  Visualization views
;; =================================================

;; This should be thread safe (?)
(defvar *viz-id-counter* 0)
(defun get-next-viz-id ()
  (format nil "viz-id-~A" (incf *viz-id-counter*)))

(defclass visualization ()
  ((dom-id :accessor dom-id :initarg :dom-id :initform (get-next-viz-id))
   (dom-css :accessor dom-css :initarg :dom-css :initform nil)
   (object :accessor visualization-object :initarg :object :initform nil)
   (query-fn :accessor query-fn :initarg :query-fn :initform nil)
   (constraints :accessor constraints :initarg :constraints :initform nil)
   (cache-style :accessor cache-style :initarg :cache-style :initform nil
		:documentation "Valid styles are :none/nil, :local, :global/t.  Local stores it only for this visualization object, global by object.")
   (dataset :accessor dataset :initarg :dataset :initform nil)
   (parameters :accessor parameters :initarg :parameters :initform nil)))

(defmethod initialize-instance :after ((viz visualization) &rest initargs)
  (declare (ignore initargs))
  (unless (parameters viz)
    (setf (parameters viz)
	  (get-default-parameters viz nil))))

(defmethod get-visualization-dataset ((viz visualization))
  "Placeholder for caching data"
  (or (get-cached-dataset viz)
      (save-visualization-dataset
       (safe-funcall (query-fn viz) (visualization-object viz) viz)
       viz)))

(defmethod get-visualization-dataset :around ((viz visualization))
  (let ((dataset (call-next-method)))
    (when (and (consp dataset) (consp (car dataset))
	       (consp (caar dataset))
	       (equal (caaar dataset) "string"))
      (setf (cdar dataset)
	    (mapcar (lambda (string)
		      (subseq string 0 (min (length string) 45)))
		    (cdar dataset))))
    dataset))
      

(defun get-cached-dataset (viz)
  (case (cache-style viz)
    (:local (dataset viz))
    (:global (or (get-cache (visualization-object viz))
		 (save-visualization-dataset 
		  (dataset viz) viz)))
    (t (dataset viz))))

(defun save-visualization-dataset (dataset viz)
  (case (cache-style viz)
    (:local (setf (dataset viz) dataset))
    (:global (setf (get-cache (visualization-object viz)) dataset)))
  dataset)

(defmethod render-visualization ((viz t) &key dataset width height)
  "Default method for standard visualizations"
  (declare (ignorable width height))
  (with-html 
    (:div :id (dom-id viz) :class "visualization"
	  :style (format nil "width: ~A; height: ~A;"
			 (or width "auto")
			 (or height "auto"))))
  (let ((dataset (json:encode-json-to-string 
		  (pre-process-google-dataset
		   (or dataset (get-visualization-dataset viz)))))
	(params (json:encode-json-alist-to-string (parameters viz))))
    (send-script
     (ps:ps* `((@ *event on-ready)
	       (lambda ()
		(set-timeout
		 (lambda ()
		   (draw-visualization ,(dom-id viz)
				       ,(string-downcase (symbol-name (type-of viz)))
				       ,dataset
				       ,params))
		 1)))))))

(defun pre-process-google-dataset (dataset)
  (when (equal (caaar dataset) "date")
    (setf (caaar dataset) "string")
    (setf (cdar dataset)
	  (mapcar #'string-utime-to-js-date
		  (cdar dataset))))
  dataset)

(defun string-utime-to-js-date (string)
  (multiple-value-bind (a b c day month year)
      (decode-universal-time (parse-integer string))
    (declare (ignore a b c day))
;;    `((v . (new (*date ,year ,month 1))) (f . ,(format nil "~A-~A" month year)))))
;;    `((new *date ,year ,month 1))
    (format nil "~A-~2,'0:D" year month)))

(defgeneric get-default-parameters (viz context)
  (:documentation "Returns default parameters based on the current
    context and the specific visualization type")
  (:method (viz context)
    (case context
      (:summary (pairs '(:width 200 :height 150 :background-color "#FFFFFF")))
      (:main-view (pairs '(:width 400 :height 300 :background-color "#FFFFFF")))
      (t (get-default-parameters viz :summary)))))


;;
;; Parameters
;;

(defmethod set-parameter ((viz visualization) name value)
  (assert (member name (valid-parameters viz)))
  (apushnew name value (parameters viz)))

(defmethod set-parameters ((viz visualization) &rest args)
  (mapcar #'(lambda (group)
	      (apushnew (first group) (second group) (parameters viz)))
	  (group args 2)))

(defmethod get-parameter ((viz visualization) name)
  (assoc-get name (parameters viz)))

(defmethod maybe-set-parameter ((viz visualization) name value)
  (when (member name (valid-parameters viz))
    (set-parameter viz name value)))


;; ================================================
;;  Interactive Pie Charts
;; ================================================

(defclass pie-chart-visualization (visualization)
  ())

(defparameter *pie-chart-valid-parameters*
  '(background-color border-color colors focus-border-color
    height is-3-d legend legend-background-color legend-text-color
    title title-color width))

(defmethod valid-parameters ((viz pie-chart-visualization))
  *pie-chart-valid-parameters*)


;; =================================================
;;  Word clouds
;; =================================================

(defclass wordcloud-visualization (visualization)
  ())

(defparameter *word-cloud-valid-parameters*
  '())

(defmethod valid-parameters ((viz wordcloud-visualization))
  *word-cloud-valid-parameters*)

(defmethod render-visualization ((viz wordcloud-visualization) &key dataset)
  "Default method for standard visualizations"
;;  (let* ((width (get-parameter viz 'width))
;;	 (height (get-parameter viz 'height)))
;;	 (style (format nil "~@[height:~A;~] ~@[width:~A;~]" height width)))
    (with-html 
      (:div :class "word-cloud-outer" ;; :style "height: 250px; width: 300px;"
;;	    (awhen (get-parameter viz 'title)
;;	      (htm (:b :style "font-size: 120%;" (str it))))
	    (:div :id (dom-id viz) :class "visualization"
		  :style "height: 300px; width: 300px;")))
;;    (let ((action-code 
;;	   (make-action (lambda (&rest args)
;;			  (declare (ignore args))
    (send-script 
     (ps:ps* `((@ *event on-ready)
	      (lambda ()
		(set-timeout
		 (lambda ()
		   (draw-visualization ,(dom-id viz)
				       ,(string-downcase (symbol-name (type-of viz)))
				       ,(json:encode-json-to-string 
					 (or dataset (get-visualization-dataset viz)))
				       ,(json:encode-json-alist-to-string (parameters viz))))
		 1))))))
;;      (send-script 
;;       (ps:ps* `(.on-load *event (lambda () (initiate-action ,action-code "")))))))



;; =================================================
;;  Bar Charts
;; =================================================

(defclass bar-chart-visualization (visualization)
  ())

(defparameter *bar-chart-valid-parameters*
;;  (mapcar #'as-keyword 
	  '(axis-color axis-background-color background-color border-color
	    colors focus-border-color height is3-d is-stacked legend
	    legend-background-color legend-text-color reverse-axis
	    title title-x title-y title-color width))

(defmethod valid-parameters ((viz bar-chart-visualization))
  *bar-chart-valid-parameters*)


;; =================================================
;;  Column Charts
;; =================================================

(defclass column-chart-visualization (visualization)
  ())

(defparameter *column-chart-valid-parameters*
;;  (mapcar #'as-keyword 
  '(axis-color axis-background-color background-color border-color
    colors focus-border-color height is-3-d is-stacked legend
    legend-background-color legend-text-color reverse-axis
    title title-x title-y title-color width))

(defmethod valid-parameters ((viz column-chart-visualization))
  *column-chart-valid-parameters*)

;; =================================================
;;  Maps
;; =================================================

(defclass map-visualization (visualization)
  ())

(defparameter *map-valid-parameters*
;;  (mapcar #'as-keyword 
	  '(show-tip enable-scroll-wheel show-line 
	    line-color line-width map-type))

(defmethod valid-parameters ((viz map-visualization))
  *map-valid-parameters*)

(defmethod render-visualization ((viz map-visualization) &key dataset)
  (with-html 
    (:div :id (dom-id viz) 
	  :class "visualization" 
	  :style "height:280px;width:380px;"))
  (send-script 
   (ps:ps* `((@ *event on-ready)
	      (lambda ()
		(draw-visualization ,(dom-id viz)
				    ,(string-downcase (symbol-name (type-of viz)))
				    ,(json:encode-json-to-string 
				      (or dataset (get-visualization-dataset viz)))
				    ,(json:encode-json-alist-to-string (parameters viz))))))))

;; =================================================
;;  Intensity Maps
;; =================================================

(defclass intensitymap-visualization (visualization)
  ())

(defparameter *intensitymap-valid-parameters*
;;  (mapcar #'as-keyword 
	  '(colors height region show-one-tab width))

(defmethod valid-parameters ((viz intensitymap-visualization))
  *intensitymap-valid-parameters*)


;; ================================================
;;  Line Chart
;; ================================================

(defclass line-chart-visualization (visualization)
  ())

;; TODO
(defparameter *line-chart-valid-parameters*
;;  (mapcar #'as-keyword 
	  '(axis-color axis-background-color background-color border-color
	    colors focus-border-color height is-3-d is-stacked legend
	    legend-background-color legend-text-color reverse-axis
	    title title-x title-y title-color width))

(defmethod valid-parameters ((viz line-chart-visualization))
  *line-chart-valid-parameters*)


;; ================================================
;;  Area Chart
;; ================================================

(defclass area-chart-visualization (visualization)
  ())

;; TODO
(defparameter *area-chart-valid-parameters*
;;  (mapcar #'as-keyword 
	  '(axis-color axis-background-color background-color border-color
	    colors focus-border-color height is-3-d is-stacked legend
	    legend-background-color legend-text-color reverse-axis
	    title title-x title-y title-color width))

(defmethod valid-parameters ((viz area-chart-visualization))
  *area-chart-valid-parameters*)


;; ============================================================
;;  Parameter sets
;; ============================================================

