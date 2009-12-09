(in-package :registry)

(registry-proclamations)

(defwidget explorer-pop-widget (widget)
  ((ids :accessor question-ids :initarg :ids :initform nil)))

(defun/cc do-create-population (pop-widget)
  (let ((population 
	 (do-dialog "Create a Population"
	   (make-instance 'create-pop-dialog
			  :questions (mapcar #'get-object 
					     (question-ids pop-widget))))))
    (when population
      (setf (question-ids pop-widget) nil))))

(defmethod render-widget-body ((w explorer-pop-widget) &rest args)
  (declare (ignore args))
  (let ((code (make-action #'(lambda (&key item &allow-other-keys)
			       (explorer-pop-handler w item)))))
    (with-html
      (:h2 #!"Population Creator")
      (render-link (f* (do-create-population w))
		   #!"Create a population")
      (:ul :id "pop-create-list" :class (unless (question-ids w) "empty")
	   (aif (question-ids w)
	       (mapc (curry #'render-pop-creator-question-item w) it)
	       (htm (str #!"(drag questions here to create a population)"))))
      (:a :id "pop-create-handler" :handler code))))
;;      (send-script "Event.onReady( function () { popCreatorInitialize() } );"))))

(defun render-pop-creator-question-item (w id)
  (let ((obj (get-object id)))
    (with-html
      (htm (:li :class "pop-question"
		(:b (str (format nil #!"Question" id)))
		(:br)
		(str (abbreviated-string (question-prompt obj)))
		(render-link (f_% (setf (question-ids w)
					(remove id (question-ids w))))
			     " [drop]"))))))

;; ITEM will be of the form "something_1234"
(defun explorer-pop-handler (w item)
  (let* ((n (position "_" item :from-end t :test #'string=))
	 (id (parse-integer item :start (1+ n))))
    (pushnew id (question-ids w))))


;; =============================================
;;  Population creation dialog
;; =============================================

(defwidget create-pop-dialog ()
  ((questions :accessor questions :initarg :questions
 	      :initform nil)
   (population :accessor population :initarg :population :initform nil)
   (presentations :accessor presentations :initarg :presentations)
   (title :accessor title :initarg :title :initform nil)
   (keywords :accessor keywords :initarg :keywords :initform nil)))

(defmethod dependencies append ((results create-pop-dialog))
  (list (make-local-dependency :stylesheet "explorer")
	(make-local-dependency :stylesheet "survey")
	(make-local-dependency :script "discovery/explorer")))

(defmethod initialize-instance :after ((pop create-pop-dialog) &rest initargs)
  (declare (ignore initargs))
  (setf (presentations pop) 
	(mapcar 'make-population-presentation 
		(questions pop)))
  (unless (title pop)
    (setf (title pop) 
	  (make-instance 'short-string-presentation 
			 :maxlength 255
			 :required t
			 :prompt "Population Name: ")))
  (unless (keywords pop)
    (setf (keywords pop) 
	  (make-instance 'short-string-presentation
			 :maxlength 255
			 :required t
			 :prompt "Keywords: "))))
  
;;
;; Handling population dialog ops
;;

(defun/cc create-pop-dialog-handler (pop args)
  (cond ((getf args :create)
	 (update-pop-dialog-presentations pop args)
;;  Don't accept duplicate populations or names
;;    (when (get-instances-by-class 'population 'name (lisp-value (title pop)))
;;      (setf (error
	 (aif (presentations-to-constraints (presentations pop))
;;    (when (constraint-set-exists constraints)
;;      (setf (error
	      (answer pop (make-population it 
					   :name (lisp-value (title pop))
					   :keywords (lisp-value (keywords pop))))
	      (progn (do-information "We do not yet support creating populations from these questions.  Please contact the administrators and tell us which questions you were trying to use.")
		     (answer pop nil))))
	(t (answer pop))))

(defmethod render-widget-body ((pop create-pop-dialog) &rest args)
  (declare (ignore args))
  (with-html-form (:post (lambda (&rest args)
			   (create-pop-dialog-handler pop args)))
    (render-pop-presentation (title pop))
    (render-pop-presentation (keywords pop))
    (dolist (presentation (presentations pop))
      (when presentation (render-pop-presentation presentation)))
    (render-create-pop-ops pop)))

(defun render-pop-presentation (presentation)
  (with-html 
    (:div :class "question"
	  (:div :class "question-prompt"
		(render-prompt presentation))
	  (:div :class "question-input"
		(render-presentation-editable presentation))
	  (:div :class "question-separator-1" "&nbsp")
	  ;;	(:div :class "question-error"
	  ;;	      (render-error p)
	  )))

(defun render-create-pop-ops (pop)
  (declare (ignorable pop))
  (render-translated-button "Create")
  (render-translated-button "Cancel"))

;;
;; Presentations for identifying populations
;;

(defun update-pop-dialog-presentations (pop args)
  (update-presentation (title pop) args)
  (update-presentation (keywords pop) args)
  (dolist (presentation (presentations pop))
    (update-presentation presentation args)))

(defmethod make-population-presentation ((question question) &rest initargs)
  (let* ((args `((:prompt ,(question-prompt question))))
	 (type (presentation-type-for-pop-question question)))
    (cond ((subtypep type 'member-choice-presentation)
	   (push (list :choices (question-choices question)) args)))
    (let ((presentation 
	   (apply #'make-instance type 
		  :metadata question 
		  :css-class "question-presentation"
		  (flatten1 (cons initargs args)))))
      (setf (dom-id presentation) (query-name presentation))
      presentation)))

(defun presentation-type-for-pop-question (question)
  (q-match-case question
    ((:boolean :checkbox)  'checkbox-boolean-presentation)
    ((:boolean :dropdown)  'dropdown-boolean-presentation)
    ((:boolean :radio)     'radio-boolean-presentation)
    ((:string :text-field) 'short-string-presentation)
    ((:string :paragraph)  'short-string-presentation)
    ((:number :text-field) 'number-presentation)
    ((:integer nil)        'range-integer-presentation)
    ((:range nil)          'range-integer-presentation)
    ((:date nil)           'date-range-presentation)
;    ((:time nil)           'time-presentation)
    ((:measurement nil)    'measurement-presentation)
;    ((:choice :dropdown)   'member-select-presentation)
;    ((:choice :radio)      'member-radio-presentation)
    ((:choice :dropdown)   'multiple-members-select-presentation)
    ((:choice :radio)      'multiple-members-select-presentation)
    ((:multichoice :multiple-dropdown)
                           'multiple-members-select-presentation)))


;; =============================================================
;;  Population selection
;; =============================================================

(defwidget population-sidebar-selector ()
  ((title :accessor title :initarg :title :initform #!"Population Filter")
   (population :accessor population :initarg :population :initform nil)))

(defmethod render-widget-body ((sidebar population-sidebar-selector) &rest args)
  (declare (ignore args))
  (with-html
    (:h2 (str (title sidebar)))
    (aif (population sidebar)
	 (htm (:b (str (abbreviated-string (population-name it))))
	      (render-link (f* (setf (population sidebar) nil)
			       (post-action-redirect 
				(request-uri-path)))
			   "&nbsp;[clear]"))
	 (render-link (f* (do-select-population sidebar))
		      "Select a population"))))


(defun/cc do-select-population (sidebar)
  (setf (population sidebar)
	(do-dialog "Select a population"
	  (make-instance 'select-pop-dialog)))
  (when (population sidebar)
;;    (mark-dirty (find-results-widget sidebar))))
    (post-action-redirect (request-uri-path)))) ;; "/dashboard/explore/")))

(defun find-results-widget (widget)
  (first (permanent-panes (get-main-widget (find-parent-workspace widget)))))

;;
;; Select population dialog
;;

(defwidget select-pop-dialog ()
  ((query :accessor query :initform 
	  (make-instance 'short-string-presentation 
			 :prompt "Population keywords"
			 :maxlength 255))
;;   (datalist :accessor datalist :initform 
;;	     (make-instance 'datalist
   (results :accessor results :initform (get-instances-by-class 'population))))

(defmethod render-widget-body ((dialog select-pop-dialog) &rest args)
  (declare (ignore args))
  (with-html-form (:post (lambda (&rest args)
			   (select-pop-dialog-handler dialog args)))
    (:div :class "form-line"
	  (:div :class "prompt" (render-prompt (query dialog)))
	  (:div :class "presentation" (render-presentation-editable (query dialog)))
	  (render-translated-button "Search"))
    (:ul :class "population"
	 (mapc (f_ (htm (:li (render-link (f* (answer dialog _))
					  (population-name _)))))
	       (results dialog)))
;;				  :title (population-tooltip result))))))
    (render-select-pop-ops dialog)))

(defun population-tooltip (population)
  (let ((strings (mapcar 'humanize-constraint (population-constraints population))))
    (apply 'concatenate 'string
	   (shuffle strings
		    (repeat "<br>" (1- (length strings)))))))
	   
	 

(defun render-select-pop-ops (dialog)
  (declare (ignore dialog))
;;  (render-button #!"Search")
  (render-translated-button "Cancel"))

(defun select-pop-dialog-handler (dialog args)
  (cond ((getf args :cancel)
	 (answer dialog nil))
	(t (update-presentation (query dialog) args)
	   (setf (results dialog)
		 (fulltext-search-for-types (lisp-value (query dialog)) '(population))))))


;;
;; Population Comparison Widget
;;

(defun make-pop-comparison-widget (comparison)
  (make-instance 'pop-comparison-widget :comparison comparison))
		 

(defwidget pop-comparison-widget ()
  ((comparison :initarg :comparison :accessor comparison)
   (viz :initarg :viz :accessor visualization :initform nil)
   (selection :initarg :selection :accessor selection :initform 0)))

(defmethod initialize-instance :after ((widget pop-comparison-widget) &rest initargs)
  (declare (ignore initargs))
  (unless (visualization widget)
    (with-slots (question main comp) (comparison widget)
      (setf (visualization widget)
	    (list (make-question-comparison-visualization question main comp)
		  (make-question-comparison-visualization2 question main comp))))))

(defmethod dependencies append ((results pop-comparison-widget))
  (list (make-local-dependency :stylesheet "explorer")
	(make-local-dependency :script "discovery/viz")
	(make-local-dependency :script "discovery/explorer")))

(defmethod render-widget-body ((widget pop-comparison-widget) &rest args)
  (declare (ignore args))
  (with-slots (question main comp) (comparison widget)
    (with-html
      (:h1 "Compare population responses")
      (:p "Main population: " (:b (str (population-name main))) "&nbsp;"
	  "Comparison population: " (:b (str (population-name comp))))
      (render-visualization (if (= (selection widget) 0) 
				(first (visualization widget))
				(second (visualization widget))))
      (render-autodropdown "selection" '(("Absolute" . 0) ("Percentage" . 1))
			   (lambda (&key selection &allow-other-keys)
			     (setf (selection widget) (parse-integer selection)))
			   :selected-value (selection widget)))))

(defun make-question-comparison-visualization (question main comp)
  (make-instance 'column-chart-visualization
		 :object question
		 :constraints (list main comp)
		 :query-fn 'comparison-query
		 :parameters (list (cons 'width 600)
				   (cons 'height 400)
				   (cons 'is3-d nil)
;;				   (cons 'background-color "#F2F2F2")
;;				   (cons 'legend-background-color "#F2F2F2")
				   (cons 'background-color "#FFFFFF")
				   (cons 'legend-background-color "#FFFFFF")
				   (cons 'legend 'right)
				   (cons 'title (question-prompt question)))))

(defun comparison-query (question viz)
  (make-categorical-comparison question 
			       (first (constraints viz))
			       (second (constraints viz))))

(defun make-question-comparison-visualization2 (question main comp)
  (make-instance 'column-chart-visualization
		 :object question
		 :constraints (list main comp)
		 :query-fn 'comparison-query2
		 :parameters (list (cons 'width 600)
				   (cons 'height 400)
				   (cons 'is3-d nil)
;;				   (cons 'background-color "#F2F2F2")
;;				   (cons 'legend-background-color "#F2F2F2")
				   (cons 'background-color "#FFFFFF")
				   (cons 'legend-background-color "#FFFFFF")
				   (cons 'legend 'right)
				   (cons 'title (question-prompt question)))))

(defun comparison-query2 (question viz)
  (make-categorical-ratio-comparison question 
			       (first (constraints viz))
			       (second (constraints viz))))

(defun make-categorical-comparison (question main comp)
  (let* (;;(all (make-categorical-distribution question :boolean nil))
	 (main-dist (make-categorical-distribution question :boolean (list main)))
	 (header (first main-dist))
	 (mdist (second main-dist))
	 (cdist (second (make-categorical-distribution question :boolean (list comp)))))
    (setf (second (car mdist)) (population-name main))
    (setf (second (car cdist)) (population-name comp))
;;    (pushlast mdist all)
    (list header mdist cdist)))
    ;;all))

(defun make-categorical-ratio-comparison (question main comp)
  (let* (;;(all (make-categorical-distribution question :boolean nil))
	 (mdist (make-categorical-distribution question :boolean (list main)))
	 (header (first mdist))
	 (mdata (second mdist))
	 (cdata (second (make-categorical-distribution question :boolean (list comp)))))
;;    (dbind (all-header1 &rest all-rest) all
      (dbind ((m-type m-title) &optional (m-data1 0) (m-data2 0)) mdata
        (declare (ignore m-title))
	(dbind ((c-type c-title) &optional (c-data1 0) (c-data2 0)) cdata
          (declare (ignore c-title))
          (list  header ;; all-header1
                 (list (list m-type (population-name main))
                       (/ m-data1 (+ m-data1 m-data2))
                       (/ m-data2 (+ m-data1 m-data2)))
                 (list (list c-type (population-name comp))
                       (/ c-data1 (+ c-data1 c-data2))
                       (/ c-data2 (+ c-data1 c-data2))))))))
    
