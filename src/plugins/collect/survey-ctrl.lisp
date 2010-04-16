(in-package :registry)

(registry-proclamations)

;; Runs a survey to completion.  
;; Survey has root question; traverses based on answers; stores answers along the way
;; One screen per root entry

;; CANDY:
;; - progress bar
;; - popup help

(defparameter *default-rows* 10)
(defparameter *default-cols* 60)

(define-plugin collect (site-app)
  "The create function accepts a list of plugins"
  :tab-name 'collect
  :create 'make-survey-viewer)

;; =========================================================
;;  Survey grid list
;; =========================================================

(defun make-survey-viewer (&rest args)
  (declare (ignore args))
  (let ((default-view (get-site-config-param :survey-viewer-default-view)))
    (apply #'make-instance 'survey-dispatcher
	   (if default-view
	       (list :default-view default-view)))))

(defwidget survey-dispatcher (dispatcher)
  ((study-list-widget :accessor study-list-widget
		      :initform (make-study-list))
   (survey-list-widget :accessor survey-list-widget
		       :initform (make-survey-grid))
   (diary-list-widget :accessor diary-list-widget
		      :initform (make-survey-grid t))
   (default-view :accessor default-view
     :initform "survey" :initarg :default-view))
  (:default-initargs :on-dispatch 'survey-dispatcher-handler))

(defmethod dependencies append ((grid survey-dispatcher))
  (list (make-local-dependency :stylesheet "survey")
	(make-local-dependency :script "survey")))

(defun return-from-survey-viewer (ctrl)
  (declare (ignore ctrl))
  (post-action-redirect "/dashboard/collect/"))

(defun goto-survey-viewer (grid survey)
  (declare (ignore grid))
  (post-action-redirect (format nil "/dashboard/collect/~A/~A" 
				(if (diary-p survey)
				    "diary"
				    "survey")
				(mid survey))))


;; TODO: DIARY LIST VIEW SUPPORT
(defmethod get-widget-for-tokens ((disp survey-dispatcher) uri-tokens)
  (let* ((tokens (remaining-tokens uri-tokens))
	 (tok (first tokens)))
    (prog1
	(cond ((equal tok "survey")
	       ;; TODO: set session variable before we get here to remember where to send user back
	       ;; (setf (default-view disp) "survey")
	       (if (rest tokens)
		   (populate-survey-ctrl disp (first (rest tokens)) uri-tokens)
		   (survey-list-widget disp)))
	      ((equal tok "diary")
	       ;; TODO: set session variable before we get here to remember where to send user back
	       ;; (setf (default-view disp) "diary")
	       (if (rest tokens)
		   (populate-survey-ctrl disp (first (rest tokens)) uri-tokens)
		   (diary-list-widget disp)))
	      ((equal tok "study")
	       ;; TODO: set session variable before we get here to remember where to send user back
	       ;; (setf (default-view disp) "study")
	       ;; TBD: get study from tokens as above for surveys and diaries
	       (study-list-widget disp))
	      (t (goto-default-survey-view disp uri-tokens)))
      (pop-tokens uri-tokens (length (remaining-tokens uri-tokens))))))

(defmethod populate-survey-ctrl (disp tok uri-tokens)
  (aif (ignore-errors (get-survey (parse-integer tok)))
       (make-survey-control it)
       (goto-default-survey-view disp uri-tokens)))

(defun goto-default-survey-view (survey-dispatcher uri-tokens)
  (declare (ignore uri-tokens))
  (get-widget-for-tokens survey-dispatcher
			 (make-instance 'uri-tokens 
					:tokens 
					(mklist (default-view survey-dispatcher)))))

      
;; =============================================================
;;  Survey Grid View
;; =============================================================

(defwidget survey-grid (datagrid) ())

(defmethod dependencies append ((grid survey-grid))
  (list (make-local-dependency :stylesheet "survey")))

(defun make-survey-grid (&optional diary-p)
  (let ((widgets
	 (list 
	  (make-widget (f* (render-survey-list-header diary-p)))
	  (make-instance 'survey-grid
			 :data-class 'survey
			 :view 'survey-viewer-view
			 :show-total-items-count-p nil
			 :allow-drilldown-p t
			 :on-drilldown (cons :do-survey 'goto-survey-viewer)
			 :autoset-drilled-down-item-p nil
			 :on-query
			 `(:filter-fn ,(lambda (survey) 
					       (not (include-survey-p survey diary-p))))
		    :sort '(sort-key . :asc)))))
    (if (get-site-config-param :survey-viewer-show-choose-patient-widget)
      (setq widgets (cons (make-choose-patient-widget :hr-p nil) widgets)))
    (make-instance 'composite :widgets widgets)))

(defun include-survey-p (survey diary-p)
  (and (current-patient)
       (or (published-p survey)
	   (is-admin-p)
	   (eq (current-user) (owner survey))
	   (member (current-user) (survey-acl survey)))
       (not (edit-lock survey))
       (or (not (get-site-config-param :survey-viewer-show-diaries-separate))
	   (and (not diary-p) (not (diary-p survey)))
	   (and diary-p (diary-p survey)))))

(defun render-survey-list-header (diary-view-p)
  (with-html 
    (:h2 (str (format nil (strcat #!"Collect Data" ": ~A")
                      (if diary-view-p #!"Diaries" #!"Surveys"))))
    ;;     (call-in-liquid-context
    ;;      (lambda ()
    ;;        (htm (:p :style "font-size: 100%;" 
    ;; 		(str #!"This section of the site is dedicated to constructing the database for LAM.  It currently focuses on background information via a set of surveys.  We will include other methods of data gathering in the near future."))
    ;; 	    (:p :style "font-size: 100%;" 
    ;; 		(str #!"Surveys consist of a collection of related questions. Surveys may contain one or more pages of questions.  You can fill these out one at a time.  You can leave a survey at any time and return later to complete it.  If you have any problems, please post a request for help in the <a href=\"/dashboard/discuss/\">Discussion Forums</a>.")))))
    (:p :class "select-view"
	(let ((diary-button-class "button")
	      (survey-button-class "button")
	      (study-button-class "button"))
	  (if diary-view-p
	      (setq diary-button-class "button-active")
	      (setq survey-button-class "button-active"))
	  (with-html
	    (:div :class "button-select"
		  (:a :href "/dashboard/collect/survey/" :class survey-button-class
		      (str #!"View Surveys"))
		  (:a :href "/dashboard/collect/diary" :class diary-button-class
		      (str #!"View Diaries"))
		  (if (get-site-config-param :survey-viewer-show-view-studies-button)
		      (htm
		       (:a :href "/dashboard/collect/study" :class study-button-class
			   (str #!"View Studies"))))))))
    (if (current-patient)
        (htm (:p (str (format nil "~A ~A." #!"Please choose from the following"
                              (if diary-view-p #!"diaries" #!"surveys")))))
        (htm (:p (str #!"Please select a patient on the Home page."))))))

(defun get-view-from-grid (grid)
  (equal (default-view (parent (parent grid))) "diary"))

(defun goto-survey-ctrl (grid survey)
  (with-flow grid
    (awhen (yield (make-survey-control survey))
      (flash-message (dataseq-flash grid) it)
      (mark-dirty (widget-parent grid)))))


;; =========================================================
;;  Survey Controller Widget
;; =========================================================

(defun make-survey-control (survey)
  (let* ((ctrl (make-instance 'survey-ctrl
			      :survey survey))
	 (ctrl-composite (make-instance 'composite :widgets
					(list ctrl))))
    ;; Returns
    ctrl-composite))

(defwidget survey-ctrl ()
  ((survey :accessor survey :initarg :survey)
   (current-id :accessor current-id :initarg :current-id :initform 1
	       :documentation "The current ID associated with answers to
                               groups in this survey")
   (survey-state :accessor survey-state)
   (group :accessor current-group :initform nil
	  :documentation "The list of current renderable questions")
   (presentations :accessor current-presentations :initarg :presentations :initform nil
		  :documentation "The presentations objects for the current group")
   (view-answers-p :accessor view-answers-p :initarg :view-answers-p :initform nil)
   (help-widget :accessor help-widget :initarg :help-widget
		:initform (make-instance 'quick-help :page "collect-help" 
					 :link-title #!"Get help here"
					 :dialog-p t)))
;;   (errors :accessor current-errors :initarg :errors :initform nil
;;	   :documentation "An alist of errors mapped to given questions by ref")
;;   (values :accessor current-values :initarg :values :initform nil))
  (:documentation "The survey widget manages a branching survey session"))

(defun initialize-control-from-state (ctrl state)
  (awhen (last-group state)
    (goto-group ctrl it)
    it))

(defmethod initialize-instance :after ((ctrl survey-ctrl) &rest args)
  (declare (ignore args))
  (initialize-control-from-state
   ctrl
   (setf (survey-state ctrl) 
 	 (or (get-survey-state (survey ctrl) (current-patient))
	     ;; No saved survey state? Go to first group in survey
	     (make-survey-state (survey ctrl) (current-patient)
				:last-group (first (survey-groups (survey ctrl)))))))
  (when (and (survey ctrl) (diary-p (survey ctrl)))
    (setf (current-id ctrl)
	  (aif (latest-answer (diary-question (survey ctrl)) (current-patient))
	       (answer-id it)
	       1)))
  ;; Traverse group / subgroup / question hierarchy and create presentations
  (create-current-presentations ctrl))

(defmethod (setf current-id) :after (id (ctrl survey-ctrl))
  ;; Diary survey? Set default value for diary question if no answer
  (let* ((survey (survey ctrl))
	 (question (diary-question survey))
	 (user (current-patient)))
    (unless (get-answer question user id)
      (add-answer question user (default-question-value question) :id id))))

(defmethod diary-p ((ctrl survey-ctrl))
  (diary-p (survey ctrl)))

(defmethod dependencies append ((obj survey-ctrl))
  (list (make-local-dependency :stylesheet "forms")))

(defun ensure-current-group (ctrl)
  "Call this to compute current-group for survey-ctrl in case it may have changed
ie if survey editor may have intervened to remove group or if survey changed (not likely)"
  (let* ((survey (survey ctrl))
	 (groups (and survey (survey-groups survey)))
	 (group (current-group ctrl)))
    ;; Returns
    (if (member group groups)
	group
	(setf (current-group ctrl) nil))))

(defmethod get-format ((ctrl survey-ctrl) key)
  (aif (survey ctrl) (get-format it key)))

(defmacro get-group-number-style-format-str (inst)
  `(case (get-format ,inst :group-number-style)
     (:roman "~@R.")
     (otherwise "~d.")))

;; Question view

(defmethod survey-object-view-url ((question question))
;;  "/"
  (format nil "/dashboard/collect/survey/~A" 
	  (aif (find-group-surveys (parent question))
	       (mid (first it))
	       (error "No survey found for question"))))

(defmethod survey-object-view-tokens ((question question))
  (list "survey" 
	(aif (find-group-surveys (parent question))
	     (format nil "~A" (mid (first it)))
	     (error "No survey found for question"))))

;; Group view

(defmethod survey-object-view-url ((group survey-group))
  (format nil "/dashboard/collect/survey/~A" 
	  (aif (find-group-surveys group)
	       (format nil "~A" (mid (first it)))
	       (error "No survey found for group"))))

(defmethod survey-object-view-tokens ((group survey-group))
  (list "survey" 
	(aif (find-group-surveys group)
	     (format nil "~A" (mid (first it)))
	     (error "No survey found for group"))))

;; Survey view

(defmethod survey-object-view-url ((survey survey))
  (format nil "/dashboard/collect/survey/~A" 
	  (mid survey)))

(defmethod survey-object-view-tokens ((survey survey))
  (list "survey" (format nil "~A" (mid survey))))



;; ===============================================================
;; Top level survey-view rendering function
;; ===============================================================

(defvar *last-survey-ctrl* nil
  "For debugging purposes")

(defmethod render-widget-body ((ctrl survey-ctrl) &rest args)
  (declare (ignore args))
  (setf *last-survey-ctrl* ctrl)
  (let ((group (ensure-current-group ctrl)))
    (unless group
      (with-html
	(:div :class "survey-left-column"
	      (render-survey-header ctrl)
	      (:h1 (str #!"This survey is empty"))
	      (:p (:a :href "/dashboard/collect/"
		      (str #!"Return to Collect")))))
      (return-from render-widget-body))
    (let* ((presentations (active-presentations ctrl group)))
      (with-html 
	(:div :class "survey-left-column"
	      (render-survey-header ctrl)
	      (htm (:h1
		    (str (format nil
				 (get-group-number-style-format-str ctrl)
				 (1+ (position group (survey-groups (survey ctrl))))))
		    (str "&nbsp;")
		    (str (slot-value-translation group 'name)))
		   (with-html-form (:post (question-response-action ctrl presentations)
					  :class "survey-form" :id "survey-form")
;;		      (render-form-actions ctrl)
		     (render-group (current-group ctrl) ctrl)
		     (render-form-actions ctrl))))
	(:div :class "survey-right-column"
	      (:div :class "top"
		    (render-widget (help-widget ctrl))
		    (render-survey-list-nav ctrl)
		    (render-survey-help ctrl))
	      (:div :class "bottom"))
	(:div :class "float-end")))))


;; ===============================================================
;;  Headers
;; ===============================================================
  
(defparameter *click-time-epsilon* (/ internal-time-units-per-second 2)
  "Ignore repeated clicks within this time interval.")

(defun render-survey-action (ctrl name title &optional class)
  "Concise rendering interface for a set of named actions"
  (setf (hunchentoot:session-value 'last-click-time)
	(get-internal-real-time))
  (labels ((do-answer (&rest args)
	     (declare (ignore args))
	     (answer ctrl nil))
	   (do-prev (&rest args)
	     (declare (ignore args))
	     ;; Kludge: try to avoid double-click lossage (see ticket:64)
	     (if (> (- (get-internal-real-time)
		       (hunchentoot:session-value 'last-click-time))
		    *click-time-epsilon*)
	       (progn
		 (setf (hunchentoot:session-value 'last-click-time)
		       (get-internal-real-time))
		 (goto-prev-group ctrl))
;;	       (log-message :lamsight :debug "ignored click in do-prev")
	       ))
	   (do-next (&rest args)
	     (declare (ignore args))
	     ;; Kludge: try to avoid double-click lossage (see ticket:64)
	     (if (> (- (get-internal-real-time)
		       (hunchentoot:session-value 'last-click-time))
		    *click-time-epsilon*)
	       (progn
		 (setf (hunchentoot:session-value 'last-click-time)
		       (get-internal-real-time))
		 (goto-next-group ctrl))
;;	       (log-message :lamsight :debug "ignored click in do-next")
	       ))
	   (do-first (&rest args)
	     (declare (ignore args))
	     (goto-first-group ctrl)))
    (render-link (ecase name
		   (answer #'do-answer)
		   (prev #'do-prev)
		   (next #'do-next)
		   (first #'do-first))
		 title
		 :class class)))
		   
(defun render-survey-header (ctrl)
  (with-html
    (:div :class "survey-bar"
          (let ((patient (current-patient)))
            (when patient
              (htm (:b "Patient: ")
                   (str (id patient))
                   (:br)(:br))))
	  (:span :class "survey-bar-title"
		 (cond
		   ((not (get-site-config-param :survey-viewer-show-diaries-separate))
		    (htm (:a :href "/dashboard/collect/" (str #!"Collect:"))))
		   ((diary-p ctrl)
		    (htm (:a :href "/dashboard/collect/diary/" (str #!"Diaries:"))))
		   (t
		    (htm (:a :href "/dashboard/collect/survey/" (str #!"Surveys:")))))
		 (:span :class "survey-bar-group-title" 
			(str (slot-value-translation (survey ctrl) 'name))))
	  (:div :class "newline survey-bar-description"
		(str (slot-value-translation (survey ctrl) 'description)))
	  (:div :class "newline survey-bar-nav"
		(if (has-prev-group ctrl)
		    (render-survey-action ctrl 'prev #!"Previous Page" "previous-group-link")
		    (htm (str #!"Previous Page")))
		" | "
		(if (has-next-group ctrl)
		    (render-survey-action ctrl 'next #!"Next Page" "next-group-link")
		    (htm (str #!"Next Page")))
		" | "
		(:a :href "/dashboard/collect/" (str #!"Return to Collect")))
	  (when (diary-p ctrl)
	    (render-diary-header ctrl))
	  (:div :style "height: 15px;")
	  (:hr))))




(defun render-diary-header (ctrl)
  (let* ((survey (survey ctrl))
	 (series (diary-question survey))
	 (description (diary-description survey)))
    (with-html
      (:div :class "newline survey-bar-diary-nav"
	    "List of entries  &nbsp; ("
	    (render-link (f* (setf (current-id ctrl) (next-id series (current-patient)))
			     (create-current-presentations ctrl))
			 #!"Add new entry")
	    ")"
	    (:ul 
	     (dolist (answer (sorted-answers series (current-patient)))
	       (render-entry-header-link ctrl answer series description)))))))

(defun render-entry-header-link (ctrl answer series description)
  (with-html 
    (:li (render-link 
	  (f* (setf (current-id ctrl) (answer-id answer))
	      (create-current-presentations ctrl))
	  description
	  :render-fn
	  (lambda (label)
	    (render-presentation
	     (make-presentation series (answer-id answer)))
	    (with-html
	      (str "&nbsp;")
	      (str (princ-to-string
		    (typecase label
		      (question (question-prompt label))
		      (otherwise label))))))
	  :class (if (eq (current-id ctrl) (answer-id answer))
		     "diary-selected-answer-entry"
		     "diary-answer-entry")))))

;; diary class w/ series and description fields?
;; Are groups 1:1 with a survey (Check if shared?)
;; support current-id in answer updates
;; how to handle rendering new entry
;; better list view; fixed height div with scrollbar
;; show | hide


;; ===============================================================
;; User feedback / comments
;; ===============================================================

(defparameter *comment-view-enabled-p* ':unknown)

(defun comment-view-enabled-p ()
  (if (eq *comment-view-enabled-p* ':unknown)
      (setq *comment-view-enabled-p* (get-site-config-param :survey-viewer-show-comments))
      *comment-view-enabled-p*))

;; ===============================================================
;;  Progress and navigation
;; ==============================================================

(defun render-survey-list-nav (ctrl)
  (with-html
    (:div :class "survey-list-nav widget"
;;	  :style "float: right;" 
	  (with-html 
	    (:h2 (str #!"Pages in Survey"))
	    (:ol 
	     (let ((counter 0.)
		   (fmt (get-group-number-style-format-str ctrl)))
	       (dolist (group (survey-groups (survey ctrl)))
		 (incf counter)
		 (let ((one-group group))
		   (htm (:li
			 (str (format nil fmt counter))
			 (str "&nbsp;")
			 (render-link
			  (lambda (&rest args)
			    (declare (ignore args))
			    (goto-group ctrl one-group))
			  (slot-value-translation one-group 'name)
			  :class (when (eq one-group (current-group ctrl))
				   "survey-list-nav-active-group"))))))))))))

(defun render-survey-help (ctrl)
  (declare (ignorable ctrl))
  (with-html
    (:div :class "survey-help widget"
;;	  :style "float: left; font-size: small;"
	  (with-html
	    (:h2 (str #!"Using the Surveys"))
	    (:ul
	     (:li 
	      (str #!"Every time you enter an answer, it is automatically saved."))
	     ;;		 (:noscript (:b (str #!"Your survey answers are only saved when you click 'Continue Survey', but not when you click on the list of pages or Previous Page or Next Page.")))
	     ;;		 (:p :class "no-noscript" (str #!"Your survey answers are automatically saved as you click on or finish entering text.  This can cause the display to jump a bit when questions are removed.")))
	     ;;		 (:script :type "text/javascript"
					;			 "$$('.no-noscript').each(function (e) { e.hide() });"))
	     (if (comment-view-enabled-p)
		 (htm
		  (:li
		   (str #!"If a question is confusing, or needs improving, you can provide feedback by clicking on the comment icon " ))))
	     (:li
	      (str #!"For the best experience, we recommend downloading and using ")
	      (:a :href "http://www.firefox.org/" (str #!"the Firefox web browser"))
	      " or " (:a :href "http://wwww.apple.com/safari" (str #!"the Safari web browser")))
	     ;;		 (:img :style "height: 15px; vertical-align: middle;" :src "/pub/images/comment-icon.jpg" :alt "COMMENT")
	     ;;		 (str #!".  If people have left comments, you can see these and decide if you want to add to them or not.") (:p))
	     ;;		(:li (:p (str #!"If you want to add a question, simply go the <a href=\"/dashboard/discuss/\">Discussion forum</a> and post a suggestion there."))))
	     )))))

		
;; ===============================================================
;;  Survey actions
;; ===============================================================

(defun render-form-actions (ctrl)
  (with-html
    (:div :class "survey-form-actions"
	  (if (has-next-group ctrl)
	      (render-button "continue" :value #!"Save and go to Next Page" 
			     :class "survey-form-button")
	      (render-button "finish" :value #!"Finish Survey" 
			     :class "survey-form-button"))
	  (when (has-next-group ctrl)
	    (render-button "finish" :value #!"Save and Finish Later" 
			   :class "survey-form-button"))
	  (when (diary-p ctrl)
	    (render-button "save" :value #!"Save This Entry"))
	  (render-button "delete" :value #!"Clear This Page" 
			 :class "survey-form-button"))))

(defun question-response-action (ctrl presentations)
  (make-action (lambda (&rest args)
		 (let ((continue (getf args :continue))
		       (finish (getf args :finish))
;;		       (restart (getf args :restart))
		       (delete (getf args :delete))
		       (update (getf args :update))
		       (save (getf args :save)))
;;		   (log-message :survey :debug "Survey form actions: ~A"
;;				(list save continue finish delete restart update))
		   (cond ((or update save)
			  (update-survey-presentations ctrl presentations args t)
			  (mark-dirty ctrl))
			 (continue
			  (update-survey-presentations ctrl presentations args t)
			  (goto-next-group ctrl)
			  (ajax-scroll-to-top))
			 (finish
			  (update-survey-presentations ctrl presentations args t)
			  (post-action-redirect "/dashboard/collect/"))
			 (delete ;; wipe answers and create new presentations
			  (delete-answers (mapcar #'metadata presentations))
			  (create-current-presentations ctrl)
			  (mark-dirty ctrl)))))))


					    
;; ===============================================================
;; Rendering a group
;; ===============================================================

(defmethod render-advice ((group survey-group) ctrl)
  (declare (ignore ctrl))
  (let ((advice (group-advice group)))
    (when advice
      (with-html
       (:div :class "survey-advice"
	     (:p :style "font-size:small;font-style:italic;" (str advice)))))))

(defmethod render-group ((group survey-group) ctrl)
  (with-html
    (:div :class "survey-group"
	  (render-advice group ctrl)
          ;;	    (when (is-admin-p)
          ;;	      (render-group-info group))
          (mapc #'(lambda (p) (render-question ctrl group p))
                (presentations-for-group ctrl group)))))

(defun render-group-info (group)
  (with-html
    (:p :style "font-size:x-small;"
	(str (format nil "[Group ID: ~A]" (mid group)))
	(loop for rule in (group-rules group) do
	     (htm (:br))
	     (str (cl-who:escape-string (format nil "~A" rule)))))))


(defmethod render-inline-group (ctrl group)
  (with-html
    (:div :class "survey-inline-group"
          (render-group group ctrl) #| 
;;	  (when (is-admin-p)
;;	    (str (format nil "[Group ID: ~A]" (mid group))))
	  (mapc #'(lambda (p) (render-question ctrl group p))
		(presentations-for-group ctrl group)) |# )))

(defmethod render-group ((group survey-group-table) ctrl)
  (with-html
    (:div :class "survey-group"
	  (render-advice group ctrl)
	  (:table
	   (dolist (row (group-table-rows group))
	     (htm
	      (:tr
	       (dolist (cell row)
		 (htm
		  (:td
		   (etypecase cell
		     (null nil)
		     (string (str cell))
		     (question
		      (htm (let ((presentation (find cell (presentations-for-group ctrl group) :key #'metadata)))
			     (validate-answers cell (current-patient))
			     (htm
			      (:div :class "question-input"
				    (render-presentation-editable presentation)))
			     #| ;; should probably refactor this code from the other render-group method
			     (let ((comment-count (comment-count cell)))
			     (htm (render-image-link (f* (do-question-comment-dialog ctrl cell))
			     "/pub/images/comment-icon.jpg" 
			     :alt #!"Add Comment"
			     :class (when (> comment-count 0)
			     "question-has-comments"))
			     (when (> comment-count 0)
			     (htm (:p :class "question-has-comments"
			     (str (format nil "(~D ~A)" comment-count
			     (if (= comment-count 1)
			     #!"comment"
			     #!"comments")))))))) |#
                         (awhen (warning-message presentation)
                           (htm (:div :class "question-error"
                                      (str it))))

                         (awhen (question-help cell)
                           (when (> (length it) 2)
                             (htm (:div :class "question-help" 
                                        ;;			   (:img :src "/pub/images/help32.png" :alt "Help")
                                        (str (slot-value-translation cell 'question-help))))))
                         ))))))))))))))


;; ===============================================================
;;  Rendering a question
;; ===============================================================

(defun render-question (ctrl group p)
  (let* ((q (metadata p))
	 (active-inline-groups 
	  (inline-groups group q (lisp-value p))))
;;    (log-message :survey :debug "render question: ~A (~A) with value ~A"
;;		 (question-name q) q (lisp-value p))
    (validate-answers q (current-patient))
    (with-html 
      (:div :class "question inline-trigger"

 	    (:div :class "question-prompt"
		  (render-prompt p)

		  (when (comment-view-enabled-p)
		    ;; Comments
		    (let ((comment-count (comment-count q)))
		      (htm (render-image-link (f* (do-question-comment-dialog ctrl q))
					      "/pub/images/comment-icon.jpg" 
					      :alt #!"Add Comment"
					      :class (when (> comment-count 0)
						       "question-has-comments"))
			   (when (> comment-count 0)
			     (htm (:p :class "question-has-comments"
				      (str (format nil "(~D ~A)" comment-count
						   (if (= comment-count 1)
						       #!"comment"
						       #!"comments"))))))))))
	    (:div :class "question-input"
		  (render-presentation-editable p))
		   
	    ;; Clear float
 	    (:div :class "question-separator-1" "&nbsp")

	    ;; Validation errors
	    (awhen (warning-message p)
	      (htm (:div :class "question-error"
			 (str it))))

	    ;; Question help
	    (awhen (question-help q)
	      (when (> (length it) 2)
		(htm (:div :class "question-help" 
;;			   (:img :src "/pub/images/help32.png" :alt "Help")
			   (str (slot-value-translation q 'question-help))))))
	    )

;; 	    (:div :class "question-prompt"
;; 		  (if (is-admin-p)
;; 		      (str (format nil "~A (~A)" (question-prompt q) (mid q)))
;; 		      (str (question-prompt q)))
;; 		  (let ((comment-count (comment-count q)))
;; 		    (htm (render-image-link (f* (do-question-comment-dialog ctrl q))
;; 					    "/pub/images/comment-icon.jpg" 
;; 					    :alt #!"Add Comment"
;; 					    :class (when (> comment-count 0)
;; 						     "question-has-comments"))
;; 			 (when (> comment-count 0)
;; 			   (htm (:p :class "question-has-comments"
;; 				    (str (format nil "(~D ~A)" comment-count
;; 						 (if (= comment-count 1)
;; 						     #!"comment"
;; 						     #!"comments")))))))))
;; 	    (:div :class "question-input"
;; 		  (render-question-input q (question-view-type q) question-value))
;; 	    (:div :class "question-separator-1" "&nbsp") 
;; 	    (awhen (assoc q (current-errors ctrl))
;; 	      (htm (:div :class "question-error"
;; ;;		         (:img :src "/pub/images/error32.png" :alt "Error!")
;; 			 (str (cdr it)))))
;; 	    (when (eq (question-data-type q) :measurement)
;; 	      (htm (:div :class "question-hint" :style "font-size: small; text-align: right;"
;; 			 (str #!"We will support English units soon.  For now you can convert units at") 
;; 			 (:a :href "http://www.worldwidemetric.com/metcal.htm"
;; 			     (str #!"this address.")))))


      (when active-inline-groups
	(dolist (group active-inline-groups)
	  (render-inline-group ctrl group)))
      (:div :class "question-separator-2" "&nbsp"))))


;; ==============================================================
;;  Question Presentations
;; ==============================================================

(defparameter *current-ctrl* nil)

(defmethod (setf current-group) :after (group (ctrl survey-ctrl))
  (declare (ignore group))
  (setf *current-ctrl* ctrl)
  (create-current-presentations ctrl))

(defun create-current-presentations (ctrl)
  "Create presentation objects for the current group"
  (when (current-group ctrl)
    (setf (current-presentations ctrl)
	  (create-group-presentations (current-group ctrl)
				      (current-id ctrl)))))

(defun create-group-presentations (group id)
  (remove-nulls
   (append (mapcar #'(lambda (q)
		       (make-presentation q id))
		   (group-questions group))
	   (mappend #'(lambda (g)
			(create-group-presentations g id))
		    (remove-nulls 
		     (mapcar #'group-rule-target (group-rules group)))))))

(defun update-survey-presentations (ctrl presentations args update-answers)
  (dolist (presentation presentations)
    (update-presentation presentation args)
    (when (and update-answers (not (warning-message presentation)))
      (update-answer (metadata presentation) (lisp-value presentation)
		     (current-id ctrl)))))

(defun active-presentations (ctrl group)
  "Determines the active / viewed presentations based on the current state"
  (let ((presentations (presentations-for-group ctrl group))
	(inline-presentations nil))
    (dolist (p presentations)
      (assert (metadata p))
      (let ((q (metadata p)))
	(dolist (group (inline-groups group q (get-question-value ctrl q)))
	  (push (active-presentations ctrl group) inline-presentations))))
    (append presentations (flatten inline-presentations))))

(defun presentations-for-group (ctrl group)
  (let ((presentations (current-presentations ctrl)))
    (remove-nulls
     (mapcar (lambda (question)
	       (find question presentations :key #'metadata))
	     (group-questions group)))))

(defun get-question-value (ctrl q)
  (aif (find q (current-presentations ctrl) :key 'metadata)
       (lisp-value it)
       (aif (get-answer q (current-patient) (current-id ctrl))
	    (value it)
	    :none)))

;; ==============================================================
;;  Navigation
;; ==============================================================

(defun has-prev-group (ctrl)
  (list-prev (current-group ctrl) (survey-groups (survey ctrl))))

(defun has-next-group (ctrl)
  (list-next (current-group ctrl) (survey-groups (survey ctrl))))

(defun goto-prev-group (ctrl)
  (goto-group ctrl (list-prev (current-group ctrl) (survey-groups (survey ctrl)))))

(defun goto-next-group (ctrl)
  (goto-group ctrl (list-next (current-group ctrl) (survey-groups (survey ctrl)))))

(defun goto-first-group (ctrl)
  (goto-group ctrl (first (survey-groups (survey ctrl)))))

(defun goto-group (ctrl group)
  (prog1
      (setf (current-group ctrl) group)
    ;; Remember this group in survey state
    (awhen (survey-state ctrl)
      (setf (last-group it) group))))


;; ==============================================================
;; Survey Preview Widget
;; ==============================================================

(defun make-page-preview-widget (group)
  (let ((preview (make-instance 'page-preview-widget 
				:survey (first (group-parents group)))))
    (setf (current-group preview) group)
    preview))

(defwidget page-preview-widget (survey-ctrl)
  ())

(defmethod dependencies append ((widget page-preview-widget))
  (list (make-local-dependency :stylesheet "survey")
	(make-local-dependency :stylesheet "forms")
	(make-local-dependency :script "survey")))

(defmethod render-widget-body ((widget page-preview-widget) &rest args)
  (declare (ignore args))
  (with-html 
    (:span :class "survey-bar-group-title" 
	   (str
	    (format nil
		    (get-group-number-style-format-str widget)
		    (1+ (position (current-group widget) (survey-groups (survey widget))))))
	   (str "&nbsp;")
	   (str (group-name (current-group widget))))
	   ;; slot-value-translation (current-group widget) 'name)))
    (render-group (current-group widget) widget)
    (render-link (f* (answer widget))
		 "Close Preview")))
