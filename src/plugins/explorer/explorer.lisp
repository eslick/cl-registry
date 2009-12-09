(in-package :registry)

(registry-proclamations)


;; ==============================================
;;  The Explorer Application
;; ==============================================


;; Sidebar:
;; - Global database filters
;; - Define populations
;; - Choose visualizations?
;; - ?

(define-plugin explorer (site-app)
  "The create function accepts a list of plugins"
  :tab-name 'explore
  :create 'make-explorer)

(defwidget explorer-workspace (workspace)
  ())

(defparameter *exp-deps* nil)

(defmethod dependencies append ((obj explorer-workspace))
  (aif-ret *exp-deps*
    (setf *exp-deps* `(,(make-local-dependency :stylesheet "explorer")
			,(make-local-dependency :stylesheet "wc")
			,(make-local-dependency :script "discovery/viz")
			,(make-local-dependency :script "discovery/explorer")
			,(make-instance 'script-dependency :url 
					(get-site-config-param :google-api-url))))))

(defun make-explorer (&rest args)
  (declare (ignore args))
  (let ((workspace (make-instance 'explorer-workspace :dom-id "explorer-workspace"))
	(tabnav (make-instance 'dynamic-navigation
			       :name "explorer"
			       :create-fn 'explorer-new-url)))
    (set-main-pane workspace 'explorer-tabnav tabnav)
    (add-pane tabnav (make-explorer-browser '(question)) ;; '(query question hypothesis))
	      '("ask-a-question") 'ask-a-question
              :title #!"Ask a Question"
	      :permanent-p t :focus-p t :redirect-p nil)
    (set-sidebar-widgets workspace
;;			 'help 
;;			 (lambda ()
;;   		            (render-explorer-help))
;;			 'share-this
;;			 (make-instance 'explorer-share-this)
;;			 'filters
;;			 (make-instance 'explorer-sidebar-filters)
			 'quick-help
			 (make-instance 'quick-help :page "explore-help" 
					 :link-title #!"Get help here"
					 :dialog-p t)
			 'main-population
			 (make-instance 'population-sidebar-selector)
			 'comp-population
			 (make-instance 'population-sidebar-selector
					:title #!"Compare Population")
			 'make-a-group 
;;			   (make-liquid-box 
			  (make-instance 'explorer-pop-widget))
    workspace))

(defun explorer-new-url (tabnav uri-tokens)
   "Acts like a dispatcher, called when no panes match, creates the pane
    and returns it"
   (let ((tokens (remaining-tokens uri-tokens)))
     (handler-case
	 (aif (explorer-object-for-tokens tokens)
	      (typecase it
		(population-comparison 
		 (do-compare-populations tabnav it)
		 (values (pane-by-uri-tokens tabnav uri-tokens)
			 tokens it nil t nil))
		(question
		 (let ((widget (make-explorer-question-view tabnav it nil tabnav)))
;;		   (add-pane tabnav it widget :tokens tokens :focus-p nil)
		   (values widget tokens nil it t t)))
		(t nil))
	      (pane-by-reference tabnav 'ask-a-question))
       (error () nil))))

(defun explorer-object-for-tokens (tokens)
  (handler-case 
      (when (= (length tokens) 2)
	(or (get-model (find-symbol (string-upcase (first tokens)) :registry)
                       (parse-integer (second tokens)))
            (error "No model found for tokens: ~s" tokens)))
    (error () nil)))

(defmethod explorer-object-edit-url ((query query))
  (format nil "/dashboard/explore/query/~A" (mid query)))

(defmethod explorer-object-edit-tokens ((query query))
  (list "query" (format nil "~A" (mid query))))

;; ==================================================
;;  Explorer Browser
;; ==================================================     

;; Generic interface for looking at different types of
;; information: queries, survey data, diaries, etc.

(defun make-explorer-browser (types)
  (make-instance 'explorer-browser :types types))

(defwidget explorer-browser ()
  ((search-bar :accessor browser-search-bar :initform nil)
   (result-viewer :accessor browser-result-viewer :initform nil)
   (view-types :accessor viewer-types :initarg :types )))


(defmethod initialize-instance :after ((widget explorer-browser) &rest initargs)
  (declare (ignore initargs))
  (let ((search (make-explorer-query-widget))
	(results (make-explorer-result-viewer)))
    (setf (browser-search-bar widget) search
	  (widget-parent search) widget
	  (browser-result-viewer widget) results
	  (widget-parent results) widget)
    ;; A default query
    (setf (viewer-results (browser-result-viewer widget))
	  (handle-browser-query (search-bar-value search) '(question)))))
	  
    

(defmethod render-widget-body ((widget explorer-browser) &rest args)
  (declare (ignore args))
  (with-transaction ()
;;   (render-link (f*
;; 		(remove-pane (widget-parent widget) 'ask-a-question)
;; 		(add-pane (widget-parent widget) 'ask-a-question
;; 			  (make-explorer-browser '(query hypotheses))
;; 			  :focus-p nil))
;; 	       "Refresh Browser")
    (render-widget (browser-search-bar widget))
    (render-widget (browser-result-viewer widget))))


;; ==================================================
;;  Explorer query widget
;; ==================================================     

(defun make-explorer-query-widget ()
  (make-search-bar (strcat #!"Enter some search terms" ":&nbsp;")
                   'do-browser-search 'ask-a-question
		   "diagnosis"))

(defun do-browser-search (search-bar query)
  "Lookup a list of objects and go to that view"
  (let ((viewer (browser-result-viewer (widget-parent search-bar)))
	(results (handle-browser-query query (viewer-types (widget-parent search-bar)))))
    (setf (viewer-results viewer) results)))

(defun handle-browser-query (query types)
  "We can cache this by keeping the intersection from the last
   query and resetting it if the query has only one word?"
  (ensure-transaction ()
    (select-if #'published-object-p 
	       (remove-duplicates
		(if (or (null query) (equal query ""))
		    (mapcan #'get-instances-by-class types)
		    (fulltext-search-for-types query types))))))

(defun fulltext-search-for-types (query types)
  (let ((sets (mapcar (lambda (term)
			(select-types (remove-nulls
				       (fulltext-search term))
				      types))
		      (mapcar 'langutils:get-lemma 
			      (filter-if #'stopword-p (split "[^\\w]+" query))))))
    (cars 
     (sort (accumulate-list (lambda (list1 list2) 
			      (search-merge-lists list1 list2))
			    sets)
	   #'>
	   :key #'cdr))))

(defun search-merge-lists (list1 list2)
  (loop for (obj . score) in (union list1 list2 :key #'first) collect
       (aif (find obj list2 :key #'first)
	    (cons obj (+ (cdr it) score))
	    (cons obj score))))

(defun select-types (objects types)
  (select-if (lambda (pair) (member (type-of (car pair)) types))
	     objects))


(defun published-object-p (object)
  "Make sure one of our parent surveys is published"
  (cond ((eq (type-of object) 'question)
	 (published-group-p (parent object)))
	((null object) nil)
	(t t)))

(defun get-explorer-constraints (widget)
  (let ((constraints nil)
	(workspace (find-parent-workspace widget)))
    (awhen (get-main-population widget workspace)
      (push it constraints))
    (awhen (get-workspace-widget workspace 'filters)
      (awhen (constraints it)
	(setf constraints (append constraints it))))
    constraints))

	   


;; (defun format-explorer-query-suggest (results)
;;   (let ((*weblocks-output-stream* (make-string-output-stream)))
;;     (declare (special *weblocks-output-stream*))
;;     (with-html
;;       (:ul :style "font-size: tiny;"
;;        (mapc (lambda (res)
;; 	       (ignore-errors
;; 		 (htm (:li (str (question-prompt res))))))
;; 	     results)))
;;     (get-output-stream-string *weblocks-output-stream*)))


;; ===================================================
;;  Summary views of various objects
;; ===================================================

;;(defwidget result-view () ())

;;(defwidget question-result-view (result-view)
;;  ((question :initarg :question :accessor question)))

(defmethod render-result-view (widget (q question))
  (let* ((constraints (get-explorer-constraints widget))
	 (viz (make-result-summary-visualization q :constraints constraints)))
    (when viz
	(with-html
	  (:div :class "exp-qsum-view" 
		;;	      (:span :style "float: right; display: inline; color: #777777; text-align: left;" "|||")
		(:div :class "exp-qsum-head" 
		      :id (format nil "question_~d" (mid q))
		      (render-link (f* (open-question-view widget q viz))
				   (slot-value-translation q 'prompt)
				   :class "exp-qsum-link")
		      (render-simple-context q))
		(:div :class "exp-qsum-detail" :style "display: none;"
		      (handler-case 
			  (progn (render-visualization viz)
				 (render-question-context q)
				 (render-question-stats q viz))
			(error () (warn "Aborting rendering of result view for ~A" q)))))))))

(defun render-simple-context (q)
  (let ((context (question-context q)))
    (with-html
      (when (cdr context)
	(htm (:span :class "exp-qctx-q2" 
		    (str (render-context-element (second context))))
	     (:span :class "exp-qctx-op"
		    (:a :href (survey-object-view-url q)
			(:img :src "/pub/images/comment-icon.jpg"))))))))

(defun render-question-context (q)
  (let ((context (nreverse (question-context q))))
    (when (cdr context)
      (with-html 
	(:div :class "exp-qctx"
	      (htm (:h2 (render-context-element (second context))))
	      (when (cddr context)
		(htm (:h3 (render-context-element (third context))))))))))

(defun render-expanded-question-context (context)
  (with-html
    (:div :class "exp-qctx"
	  (mapcar (lambda (ctx)
		    (with-html 
		      (:h2 (render-context-element ctx))))
		  (nreverse (subseq context 1))))))

(defun render-context-element (element)
  (with-html
    (typecase element
      (question 
       (htm (str #!"under question") " " 
	    (:a :href (survey-object-view-url element)
		(str (slot-value-translation element 'prompt)))))
      (survey-group 
       (htm (str #!"on page") " " 
	    (:a :href (survey-object-view-url element)
		(str (slot-value-translation element 'name)))))
      (survey 
       (htm (str #!"in survey") " " 
	    (:a :href (survey-object-view-url element)
		(str (slot-value-translation element 'name))))))))

(defmethod make-result-summary-visualization (question &key type quiet constraints)
  (let ((type (or type (default-summary-visualization-type question))))
    (cond ((member type
		   '(pie-chart-visualization bar-chart-visualization))
	   (make-instance type
			  :object question
			  :constraints constraints
			  :query-fn 'distribution-query
			  :parameters (list (cons 'width  200)
					    (cons 'height 140)
					    (cons 'is3-d nil)
					    (cons 'background-color "#FFFFFF")
;;					    (cons 'background-color "#F2F2F2")
					    (cons 'legend-background-color "#FFFFFF")
;;					    (cons 'legend-background-color "#F2F2F2")
					    (cons 'legend 'bottom)
					    (cons 'title (question-prompt question)))))
	  ((eq type 'column-chart-visualization)
	   (make-instance type
			  :object question
			  :constraints constraints
			  :query-fn 'distribution-query
			  :parameters (list (cons 'width  200)
					    (cons 'height 140)
					    (cons 'is3-d nil)
					    (cons 'title-x 
						  (if (eq (question-data-type question)
							  :measurement)
						      (get-locale-unit
						       (canonical-unit-for-measurement
							(question-data-subtype question)))
						      ""))
					    (cons 'background-color "#FFFFFF")
;;					    (cons 'background-color "#F2F2F2")
					    (cons 'legend-background-color "#FFFFFF")
;;					    (cons 'legend-background-color "#F2F2F2")
					    (cons 'legend 'bottom)
					    (cons 'title (question-prompt question)))))
	  ((eq type 'line-chart-visualization)
	   (make-instance type
			  :object question
			  :query-fn 'histogram-query
			  :parameters (list (cons 'width 200)
					    (cons 'height 150)
					    (cons 'title-x (if (eq (question-data-type question)
								    :measurement)
								(get-locale-unit
								 (canonical-unit-for-measurement
								  (question-data-subtype question)))
								""))
					    (cons 'background-color "#F2F2F2")
					    (cons 'legend-background-color "#F2F2F2"))))
	  ((eq type 'series-visualization)
	   (make-instance 'line-chart-visualization
			  :object question
			  :query-fn 'series-query
			  :parameters (list (cons 'width 200)
					    (cons 'height 140)
					    (cons 'title-x 
						  (if (eq (question-data-type question)
							  :measurement)
						      (get-locale-unit
						       (canonical-unit-for-measurement
							(question-data-subtype question)))
						      ""))
					    (cons 'background-color "#F2F2F2")
					    (cons 'legend-background-color "#F2F2F2"))))
	  ((eq type 'wordcloud-visualization)
	   (make-instance type
			  :dataset (unless quiet
				     (make-word-cloud-dataset
				      (get-answer-values* question constraints)))
			  :cache-style :local
			  :parameters (list (cons 'width 140)
					    (cons 'height 140)
					    (cons 'title (question-prompt question))))))))

(defmethod default-summary-visualization-type (question)
  (when (and (diary-question-p question)
	     (member (question-data-type question) '(:number :measurement)))
    (return-from default-summary-visualization-type 
      'series-visualization))
  (case (question-data-type question)
    (:boolean 'pie-chart-visualization)
    (:choice 'bar-chart-visualization)
    (:multichoice 'bar-chart-visualization)
    (:string 'wordcloud-visualization)
    (:date 'line-chart-visualization)
    (:number 'line-chart-visualization) ;; 'histogram-column-chart-visualization)
    (:measurement 'line-chart-visualization) ;; 'histogram-column-chart-visualization)
    (t 'bar-chart-visualization)))

(defmethod render-question-stats (question viz)
  (with-html
    (:div :class "explorer-question-stats"
	  (:p :class "question-respondents"
	      (str (simple-coverage-stat question viz))))))

(defun simple-coverage-stat (question viz)
  (format nil "~A / ~A~A" 
	  (length (get-filtered-answers question (constraints viz)))
	  (length (compute-user-oids (constraints viz)))
	  #!" of patients responded"))

;; ===================================================
;;  Question View
;; ===================================================

(defmethod explorer-object-edit-url ((question question))
  (format nil "/dashboard/explore/question/~A" (mid question)))

(defmethod explorer-object-edit-tokens ((question question))
  (list "question" (format nil "~A" (mid question))))

(defun replace-question-view (widget question viz)
  (declare (ignore viz))
  (let ((tabnav (find-workspace-widget widget 'explorer-tabnav)))
;;    (remove-pane tabnav question)
    (do-pane tabnav question 
	     (make-explorer-question-view tabnav question nil tabnav)
	     (explorer-object-edit-tokens question))))

(defun open-question-view (widget question viz)
  (declare (ignore viz))
  (let ((tabnav (find-workspace-widget widget 'explorer-tabnav)))
    (do-pane tabnav question 
	     (make-explorer-question-view tabnav question nil tabnav)
	     (explorer-object-edit-tokens question)
             (slot-value-translation question 'prompt))))

(defun make-explorer-question-view (tabnav question viz wksp-widget)
  (declare (ignore tabnav))
  (make-instance 'explorer-question-view 
		 :question question
		 :visualization viz
		 :workspace-ref wksp-widget))

(defwidget explorer-question-view ()
  ((question :initarg :question :accessor question)
   (viz :initarg :visualization :accessor view-viz)
   (ref :initarg :workspace-ref :accessor workspace-ref)))

(defmethod initialize-instance :after ((view explorer-question-view) &rest initargs)
  (declare (ignore initargs))
  (let ((constraints (get-explorer-constraints (workspace-ref view))))
    (aif (view-viz view)
	 (let ((new (make-result-summary-visualization (question view)
						       :constraints constraints)))
	   (setf (dataset new) (dataset it))
	   (setf (view-viz view) new))
	 (setf (view-viz view) 
	       (make-result-summary-visualization (question view)
						  :constraints constraints))))
  (let ((viz (view-viz view)))
    (maybe-set-parameter viz 'width 640)
    (maybe-set-parameter viz 'height 500)
    (maybe-set-parameter viz 'background-color "#FFFFFF")
    (maybe-set-parameter viz 'text-x "#F0EEF4")
    (maybe-set-parameter viz 'text-y "#F0EEF4")
    (maybe-set-parameter viz 'legend-background-color "#FFFFFF")
;;    (maybe-set-parameter viz 'colors '("#FFFFFF" "#000000" "#FF00FF"))
;;    (maybe-set-parameter viz 'legend-background-color "#F0EEF4")
    (maybe-set-parameter viz 'is3-d nil)))

(defmethod render-widget-body ((view explorer-question-view) &rest args)
  (declare (ignore args))
  ;; Update constraints with latest state
  (setf (constraints (view-viz view)) 
	(get-explorer-constraints (workspace-ref view)))
  ;; Render
  (with-html
    (:h1 :id "explorer-expanded-header"
	 (str #!"Exploring: ") (str (slot-value-translation (question view) 'prompt)))
    (:p (render-link (f* (answer view)) #!"[Close this View]")
     	(when (and (comparison-population-selected-p view)
		   (get-main-population view))
	  (htm (str "&nbsp;"))
	  (render-link (f* (compare-view-populations view))
		       #!"[Compare Populations]")))
    (:div :class "explorer-expanded-viz-select"
	  (let ((selections (get-visualization-selections 
			     (question-data-type (question view)))))
	    (when selections
	      (htm 
	       (:span :class "explorer-expanded-viz-picker"
		      (str #!"Select a visualization: ")
		      (render-autodropdown "viz-type" 
					   selections
					   (lambda (&rest args)
					     (question-viz-update-handler view args))
					   :selected-value
					   (get-client-visualization-selection (type-of 
										(view-viz view)))))))))
    (:div :class "viz-header"
	  (awhen (question-context (question view))
	    (htm (:b (str #!"Question Context")))
	    (render-expanded-question-context it))
	  (render-question-stats (question view) (view-viz view)))
    (:div :class "explorer-expanded-visualization"
	  (render-visualization (view-viz view)))
    (:div :class "related-questions"
	  (:p (:b (str #!"Related questions")))
	  (mapcar #'(lambda (obj)
		      (render-link (f* (replace-question-view view obj (view-viz view)))
				   (question-prompt obj)
				   :id "explorer-expanded-interior")
		      (with-html (:br)))
		  (let ((questions (related-questions-in-context (question view))))
		    (safe-subseq questions 0 9))))))
;;    (send-script "explorerQuestionViewInitialize()")))
	  


(defmethod question-viz-update-handler ((view explorer-question-view) args)
  (awhen (getf args :viz-type)
    (setf (view-viz view)
	  (switch-visualization (view-viz view) (question view) 
				(get-lisp-visualization-type it)))))
			       
;;    (with-html-form (:get (f* (question-view-action-handler view)))

(defun get-main-population (widget &optional workspace)
  (let ((ref (or workspace (find-parent-workspace widget))))
    (awhen (get-workspace-widget ref 'main-population)
      (population it))))

(defun get-comp-population (widget &optional workspace)
  (let ((ref (or workspace (find-parent-workspace widget))))
    (awhen (get-workspace-widget ref 'comp-population)
      (population it))))

(defun comparison-population-selected-p (view)
  (get-comp-population view))

(defun compare-view-populations (view)
  (let ((comparison (make-comparison-from-view view))
	(tabnav (find-workspace-widget view 'explorer-tabnav)))
    (do-compare-populations tabnav comparison)))
   

(defun make-comparison-from-view (view)
  (let* ((workspace (find-parent-workspace view))
	 (main (get-main-population view workspace))
	 (comp (get-comp-population view workspace)))
    (make-population-comparison (question view) main comp)))

(defun do-compare-populations (tabnav comparison)
  (do-pane tabnav comparison
	   (make-pop-comparison-widget comparison)
	   (explorer-object-edit-tokens comparison)))

(defmethod explorer-object-edit-url ((comp population-comparison))
  (format nil "/dashboard/explore/comparison/~A" (mid comp)))

(defmethod explorer-object-edit-tokens ((comp population-comparison))
  (list "comparison" (format nil "~A" (mid comp))))



			   
;;
;; Views
;;

;; Need: date, time and measurement support    
(defparameter *valid-visualizations-for-questions*
  '((:string wordcloud-visualization)
    (:boolean bar-chart-visualization column-chart-visualization pie-chart-visualization)
    (:number line-chart-visualization column-chart-visualization bar-chart-visualization )
    (:measurement line-chart-visualization column-chart-visualization bar-chart-visualization )
    (:choice bar-chart-visualization column-chart-visualization pie-chart-visualization)
    (:multichoice bar-chart-visualization column-chart-visualization)))

(defun get-visualization-selections (dtype)
  (mapcar #'get-client-visualization-selection
	  (cdr (assoc dtype *valid-visualizations-for-questions*))))

(defparameter *visualization-names*
  '((wordcloud-visualization . "Word Cloud")
    (bar-chart-visualization . "Bar Chart")
    (column-chart-visualization . "Column Chart")
    (pie-chart-visualization . "Pie Chart")
    (line-chart-visualization . "Line Chart")))

(defun get-client-visualization-selection (viz-type)
  (cdr (assoc viz-type *visualization-names*)))

(defun get-lisp-visualization-type (client-name)
  (car (find client-name *visualization-names* :key #'cdr :test #'equalp)))

(defun switch-visualization (viz object new-type)
  (let ((new-viz (make-result-summary-visualization 
		  object :type new-type :quiet t)))
    (setf (dom-id viz) (get-next-viz-id))
    (setf (dataset new-viz) (dataset viz))
    (loop for parameter in (parameters viz) do
	 (maybe-set-parameter new-viz (car parameter) (cdr parameter)))
    new-viz))

(defun copy-visualization (viz)
  (switch-visualization viz (visualization-object viz) (type-of viz)))
			    
;;
;; Filtering
;;

;; Expand fulltext search to support union of conjunction is too small
;; Page view in search?

(defwidget explorer-sidebar-filters ()
  ((constraints :accessor constraints :initarg :constraints :initform nil)
   (presentations :accessor presentations :initarg :presentations :initform nil)))

(defmethod initialize-instance :after ((sidebar explorer-sidebar-filters) &rest initargs)
  (declare (ignore initargs))
  (when (constraints sidebar)
    ;; generate presentations
    ))

(defmethod render-widget-body ((sidebar explorer-sidebar-filters) &rest args)
  (declare (ignore args))
  (with-html
    (:h2 "Global Filters")
    (:div "Patients only" (present-as 'checkbox-boolean-presentation t))
    (:div "Trusted only" (present-as 'checkbox-boolean-presentation nil))))

;; NOTE: need handler and ajax auto update

;;
;; Browsing
;;

;; Related questions (same inline group, enabled subgroup)

;; ===================================================
;;  Queries
;; ===================================================

;;(defmethod render-result-view (widget (q query))
;;  (with-html
;;    (str (query-text q))))

;;(defmethod render-result-view ((survey survey))
;;  "Simple survey view - draggable"
;;  (with-html
;;    (str (name survey))))

;;(defmethod render-result-view ((group survey-group))
;;  "Simple group view - draggable"
;;  (with-html
;;    (str (group-name group))))

;;(defmethod render-result-view ((q database-query))
;;  (with-html
;;    (str (query-text q))))



;; ======================================================
;;  Explorer query view
;; ======================================================

;; (defmethod make-explorer-view ((query query))
;;   (make-instance 'explorer-query-widget :query query))

;; (defwidget explorer-query-widget ()
;;   ((query :accessor query :initarg :query)))

;; (defmethod render-widget-body ((widget explorer-query-widget) &rest args)
;;   (declare (ignore args))
;;   (let ((query (query widget)))
;;     (with-html
;;       (:h1 (query-text query))
;;       (:p "This query is answered using the following patient populations")
;;       (mapc #'render-population-link (query-populations query))
;;       (:hr)
;;       (mapc (curry 'render-query-view query) (query-views query))))
;;   (render-link (f* (answer widget)) "Return"))

;; (defun render-population-link ((population population))



;; ==================================================
;;  Explorer results viewer
;; ==================================================     

(defun make-explorer-result-viewer ()
  (make-instance 'explorer-results-viewer))

(defwidget explorer-results-viewer ()
  ((results :accessor viewer-results :initarg :results :initform nil)
   (datalist :accessor viewer-results-list :initform nil)))

(defmethod initialize-instance :after ((results explorer-results-viewer) &rest initargs)
  (declare (ignore initargs))
  (setf (viewer-results results)  nil)
;;	(get-instances-by-class 'query))
;;   (setf (viewer-results-list results)
;; 	(make-instance 'datagrid 
;; 		       :data-class 'query
;; 		       :on-query (lambda (widget &key countp &allow-other-keys)
;; 				   (declare (ignore widget))
;; 				   (if countp 
;; 				       (length (viewer-results results))
;; 				       (viewer-results results)))
;; 		       :view 'results-table-view
;;  		       :allow-sorting-p nil
;; 		       :on-drilldown (cons :view 'view-query)
;; 		       :allow-drilldown-p t))
;;   (setf (widget-parent (viewer-results-list results)) results)
  results)

;; (defview results-table-view (:type table)
;;   (query-or-survey-question :reader #'results-title))

;; (defun view-query (widget item)
;;   (let ((tabnav (find-workspace-widget widget 'explorer-tabnav)))
;;     (do-pane tabnav item (make-explorer-view item)
;; 	     (explorer-object-edit-tokens item))))

(defmethod render-widget-body ((widget explorer-results-viewer) &rest args)
  (declare (ignore args))
  (with-html
    (:h1 (str #!"Search Results"))
;;    (render-widget (viewer-results-list widget))))
    (:div :class "result-list"
	  (if (viewer-results widget)
	      (htm (:ul
		    (with-transaction ()
		      (mapc (curry #'render-result-view widget)
			    (explorer-page-results (viewer-results widget))))))
	      (htm (:p "No search results"))))))
;;    (send-script "explorerResultViewInitialize()")))

(defparameter *results-per-page* 10)

(defun explorer-page-results (results &optional (page-num 1))
  (assert (= page-num 1))
  (safe-subseq results 0 *results-per-page*))

