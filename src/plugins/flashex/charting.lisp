(in-package :flashex)

(define-api-handler (fx-chart :mime "text/html") (token params auth-p)
  (let ((question (assoc-get :query params))
	(population (assoc-get :pop params))
	(style (assoc-get :style params)))
    (with-html
      (json:encode-json-to-string 
       (if (equal question "test")
	   (map-chart *chart1*)
	   (awhen (parse-integer question)
	     (get-summary-chart it population style)))))))

(defparameter *current-skin*
  '((:bg-color . "#FFFFFF")
    (:grid-color . "#BBBBBB")
    (:title-style . "{font-size: 12px; color: #9970EE; font-family: Helvetica;}")
    (:y-legend-style . "{font-size: 12px; color: #9970EE; font-family: Helvetica;}")
    (:axis-stroke . 1)
    (:y-axis-color . "#d00d00")
    (:x-axis-color . "#D00D00")
    (:x-axis-tick . 10)))


(defun get-skin-selection (type)
  (assoc-get type *current-skin*))

;;
;; Turn a question into a dataset for the charting widget
;;

(defun get-summary-chart (qid popstring &optional (type "bar") catch-error-p)
  (handler-case 
      (let ((question (get-question qid))
	    (popid (when popstring (parse-integer popstring))))
	(remove-nulls
	 (append (list `(bg_colour . ,(get-skin-selection :bg-color))
		       (make-qchart-title question)
		       (make-qchart-y-legend question))
		 (make-qchart-series question type))))
    (error (e) 
      (if catch-error-p nil
	  (signal e)))))

(defun make-qchart-title (question)
  `(title (text . ,(question-prompt question))
	  (style . ,(get-skin-selection :title-style))))
			       

(defun make-qchart-y-legend (question)
  (declare (ignorable question))
  `(y_legend (text . "Number of Patients")
	     (style . ,(get-skin-selection :y-legend-style))))


;;;; SERIES


(defun make-qchart-series (question type)
  (case (question-data-type question)
    (:boolean (make-pie-series question))
    (:choice (make-bar-series question))
    (:multichoice (make-bar-series question))
    (:number (make-bar-series question))
    (:string (make-word-viz question))
    (t (make-line-series question))))

(defun get-discrete-data (question)
  (let ((dist (make-categorical-distribution question (question-data-type question) nil)))
    `((labels . ,(rest (first dist)))
      (values . ,(rest (second dist))))))

(defun make-qchart-y-axis (question values)
  (declare (ignorable question))
  `(y_axis (stroke . 4)
	   (colour . ,(get-skin-selection :y-axis-color))
	   (grid_colour . ,(get-skin-selection :grid-color))
	   (max . ,(* 1.2 (cond ((null values)
				 10)
				((> (length values) 1)
				 (apply #'max values))
				(t (first values)))))))

(defun make-qchart-x-axis (question labels &key rotate)
  (declare (ignorable question))
  `(x_axis (stroke . ,(get-skin-selection :axis-stroke))
	   (tick_height . ,(get-skin-selection :x-axis-tick))
	   (colour . ,(get-skin-selection :x-axis-color))
	   (grid_colour . ,(get-skin-selection :grid-color))
	   (rotate . ,(aif-ret rotate 0))
	   (labels . ,labels)))

(defun make-qchart-x-legend (question labels)
  `(x_legend (text . responses)
	     (style . "{color: #5368CC; font-size: 14px;}")))

;;;; Discrete Charts

(defun make-pie-series (question)
  (let ((data (get-discrete-data question)))
    `(,(make-qchart-x-axis question (assoc-get 'labels data))
       ,(make-qchart-x-legend question nil)
       ,(make-qchart-y-axis question (assoc-get 'values data))
       (elements ((type . pie)
		  (alpha . 0.7)
		  (animate . 0)
		  (start-angle . 180)
		  (colours "#D15600" "#43B020");; "#C78910" "#73880A", "#D15600", "#6BBA70")
		  (values ,@(mapcar (lambda (l v)
				      `((value . ,v) (label . ,l)))
				    (mapcar #'humanize-name (assoc-get 'labels data))
				    (assoc-get 'values data))))))))

(defun make-bar-series (question)
  (let ((data (get-discrete-data question)))
    `(,(make-qchart-x-axis question 
			   (mapcar #'humanize-name (assoc-get 'labels data)))
       ,(make-qchart-x-legend question nil)
       ,(make-qchart-y-axis question (assoc-get 'values data))
       (elements ((type . bar)
		  (alpha . 0.7)
		  (colour . "#D15600")
		  (text . "Answers")
		  (font-size . "10")
		  (values ,@(assoc-get 'values data)))))))
		    


;;;; Line Charts

(defun make-line-series (question)
  (let* ((data (make-histogram-distribution question (question-data-type question) nil))
	 (labels (let ((labels (rest (first data))))
		   (if (eq (question-data-type question) :date)
		       (mapcar #'string-utime-to-js-date labels)
		       labels)))
	 (values (rest (second data))))
    `(,(make-qchart-x-axis question labels)
       ,(make-qchart-x-legend question nil)
       ,(make-qchart-y-axis question values)
       (elements ((type . line)
		  (alpha . 0.7)
		  (colour . "#D15600")
		  (text . "Answers")
		  (font-size . "10")
		  (values ,@values))))))
		    

(defun convert-discrete-argument (value)
  (cond ((eq value t) "True")
	((null value) "False")
	(t (registry::humanize-name value))))
    

;;;; Strings

(defun make-word-viz (question)
  (let ((data (string-question-distribution question)))
    `(,(make-qchart-x-axis question 
			   (mapcar #'humanize-name (assoc-get 'labels data))
			   :rotate "diagonal")
       ,(make-qchart-x-legend question nil)
       ,(make-qchart-y-axis question (assoc-get 'values data))
       (elements ((type . bar)
		  (alpha . 0.7)
		  (colour . "#D15600")
		  (text . "Answers")
		  (font-size . "10")
		  (values ,@(assoc-get 'values data)))))))

(defun string-question-distribution (question &optional (truncate 20))
  (let* ((dset 
	 (convert-to-google-dataset
	  (list (get-column-header question)
		'("number" "Number of Respondents"))
	  (cons->list (compute-distribution 
		       (filter-if #'stopword-p
				  (flatten (mapcar #'extract-words 
						   (get-answer-values* question nil))))
		       :test #'equal))))
	 (labels (truncate-list (rest (first dset)) truncate))
	 (values (truncate-list (rest (second dset)) truncate)))
    `((labels ,@labels)
      (values ,@values))))

(defun truncate-list (list limit)
  (subseq list 0 (min (length list) limit)))
