(in-package :registry)

(registry-proclamations)

;; =========================================================
;;  The survey editor is a workspace with an embedded nav
;; =========================================================

(define-plugin editor (site-app)
  "The create function accepts a list of plugins"
  :tab-name 'create
  :create 'make-survey-editor)


(defun make-survey-editor (&rest args)
  (declare (ignore args))
  (let ((workspace (make-instance 'workspace :dom-id "survey-editor"))
	(tabnav (make-instance 'dynamic-navigation 
		  :name "editor"
		  :create-fn 'survey-editor-new-uri)))
;;	(actions (repeat '("Test link to forums" forum-link) 4)))
    (set-main-pane workspace 'editor-tabnav tabnav)
    (add-pane tabnav (make-survey-editor-list)
	      '("survey-list") 'survey-list
	      :permanent-p t :focus-p t :redirect-p nil)
    (set-sidebar-widgets workspace 
       'survey-editor-quick-help
       (make-instance 'quick-help :page "create-help" 
		      :link-title "Get help here"
		      :dialog-p t)
       'survey-editor-actions (make-editor-action-panel)
       'scratchpad (make-scratchpad :dom-id "scratchpad")
       'help (make-widget
	      (lambda (&rest args)
		(declare (ignore args))
		(with-html
		  (:h2 (str #!"Editor Help"))
		  (:p :style "font-size: small;" 
		      (str #!"The survey editor will require some time to become familiar with.  It allows you to create, organize, and edit all elements of a survey.  We will be expanding this help section after some feedback from users and some more improvements in the features and the look."))
		  (:p :style "font-size: small;"
		      (str #!"The Scratchpad is a place to store groups and questions if you want to move them from one editor to another.  A question can only have one parent, so when you drag it back to a different group, it will disappear from the first."))))))
    workspace))

(defun update-search-actions (self actions)
  "Find the sidebar-search-actions widget and update the actions therein.
   ACTIONS should be a list of (string function-designator) pairs."
  ;; Don't do anything here for the moment.
  (return-from update-search-actions)
  (let ((search-actions (find-workspace-widget self 'search-actions)))
    (setf (sidebar-actions search-actions) actions)))

;;;; Two ways to navigate to survey editor views

(defun forum-link (widget)
  (declare (ignore widget))
  (post-action-redirect "/dashboard/discuss"))

;;(defgeneric survey-object-edit-url (object)
;;  (:documentation "Returns a URL that takes you to an editor view,
;;    creating the pane if necessary.  One of two ways to get to edit views"))
     
;; =========================================================
;;  Dispatch on URI
;; =========================================================

(defun survey-editor-new-uri (tabnav uri-tokens)
  "Acts like a dispatcher, called when no panes match, creates the pane
   and returns it"
  (let ((tokens (remaining-tokens uri-tokens)))
    (handler-case 
	(let* ((id (parse-integer (second tokens)))
	       (object (if (equal (first tokens) "survey")
			   (get-survey id)
			   (get-group id)))
	       (widget (make-survey-editor-view tabnav object)))
	  (values widget tokens nil object t t))
      (error () nil))))

;; =========================================================
;; basic permissions checking
;; =========================================================

(defmethod editable-by-user-p ((s survey))
  (or (is-admin-p)
      (is-editor-p)
      (eq (current-user) (owner s))
      (member (current-user) (survey-acl s))))

(defmethod editable-by-user-p ((g survey-group))
  (or (is-admin-p)
      (is-editor-p)
      (eq (current-user) (owner g))
      (member (current-user) (group-acl g))
      (captive-survey-editor-p g)))

(defmethod editable-by-user-p ((q question))
  (editable-by-user-p (parent q)))

(defun captive-survey-editor-p (group)
  (let ((surveys (find-all-group-surveys group)))
    (when (= (length surveys) 1)
      (editable-by-user-p (first surveys)))))


;; ==================================================
;; undo support
;; ==================================================

;;; A session-object allows objects with uncommitted edits to be
;;; visible outside of a particular editing view.

(defun session-edited-objects ()
  (hunchentoot:session-value 'edited-objects))

(defun session-object (thing)
  (or (gethash thing (session-edited-objects))
      thing))

(defun (setf session-object) (new thing)
  (setf (gethash thing (session-edited-objects)) new))

(defun clear-session-object (thing)
  (remhash thing (session-edited-objects)))

(defclass undo-redo-mixin ()
  ((undo-history :accessor undo-history :initform nil)
   (redo-history :accessor redo-history :initform nil)))

(defmethod clear-undo-state ((obj undo-redo-mixin))
  (setf (undo-history obj) nil
	(redo-history obj) nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;           VIEWS                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; =========================================================
;;  Survey List View
;; =========================================================

(defwidget survey-edit-grid (datagrid)
  ())

(defun render-survey-edit-grid-header ()
  (with-html 
    (htm
     (:h1 (str #!"Creating Surveys"))
;;	(:p :style "font-size: 80%;" 
;;		(str #!"The goal of this site is not to be static.  We want to enable questions to change over time, to ask clarifying questions, and collect new information.  The survey editor is a key part of the 'living database' concept behind LAMsight.  The editor will evolve over time in response to your comments.  Creating new questionnaires is not for everyone, but comments you leave in the forums or in question comments can inform those who are editing and creating surveys."))
     (:p :style "font-size: 80%;" 
	 (str #!"The survey editor consists of a set of <b>views</b>.  Each view has its own tab at the top of the screen.  Surveys are made up of <b>groups</b>, each group is made up of <b>questions</b>.  There is a view that allows you to see, review and change each of these components of a survey.  Please post your questions in the forums so we can respond to the most important questions.")))
    (:p (str #!"Please choose from one of the following surveys or create your own!"))))


(defun make-survey-editor-list ()
  (make-instance 'survey-edit-grid
		 :data-class 'survey
		 :on-query (list :filter-fn #'(lambda (x)
						(not (editable-by-user-p x))))
		 :view 'survey-simple-view
		 :allow-drilldown-p t
		 :on-drilldown (cons :do-survey 'edit-survey-object)
		 :autoset-drilled-down-item-p nil
		 :show-total-items-count-p nil
		 :sort '(name . :asc)))

(defmethod render-dataseq-body :before ((grid survey-edit-grid) &rest args)
  (declare (ignore args))
  (render-survey-edit-grid-header))

(defview new-survey-form-view (:type form :caption "Create new survey")
  name
  (description :present-as (textarea :cols 60 :rows 10)))

(defmethod render-widget-body :after ((grid survey-edit-grid) &rest args)
  (declare (ignore args))
  (with-html
    (:p
     (if (current-center)
         (render-link (f* (do-dialog "" (make-quickform 'new-survey-form-view
                                                        :data-class-name 'survey
                                                        :on-success
                                                        (lambda (qform temp)
                                                          (declare (ignore qform))
                                                          (setq temp (persist-object *default-store* temp))
                                                          (setf (published-p temp) nil)
                                                          (setf (owner temp) (current-user))
                                                          (mark-dirty grid)))))
                      #!"[Create New Survey]")
         (str #!"To create a new survey, you must choose a center on the Home tab.")))))
						  

(defun edit-survey-object (grid survey)
  ;; Use workspace lookup here instead?
  (let ((tabnav (find-workspace-widget grid 'editor-tabnav)))
    (unless (pane-by-reference tabnav survey)
      (do-pane tabnav survey (make-survey-editor-view tabnav survey)
	       (survey-object-edit-tokens survey)))))

;; =========================================================
;;  Survey Objects and Edit View
;; =========================================================

(defmethod survey-object-edit-url ((survey survey))
  (format nil "/dashboard/create/survey/~A" (mid survey)))

(defmethod survey-object-edit-tokens ((survey survey))
  (list "survey" (format nil "~A" (mid survey))))

(defmethod make-survey-editor-view (nav (survey survey)) ;; workspace)
  (if (and (edit-lock survey)
	   (not (eq (edit-lock survey) hunchentoot:*session*)))
      (make-widget
       (lambda (&rest args)
	 (declare (ignore args))
	 (log-message :survey :debug "Attempted to edit locked survey ~s"
		      survey)
	 (with-html
	   (:p "It appears that this survey is being edited by someone else.")
	   (:p "Please try again later."))))
      (progn
	(setf (edit-lock survey) hunchentoot:*session*)
	(register-on-close-callback nav survey
				    (lambda (ref)
				      (declare (ignore ref))
				      (setf (edit-lock survey) nil)))
	(let ((sw (make-survey-editor-widget 
		   survey
		   (find-workspace-widget nav 'survey-editor-actions))))
	  (setf (widget-parent sw) nav)
	  sw))))

;; sidebar actions for survey editor

(defun survey-add-group (widget)
  (declare (ignore widget))
  (do-information "survey-add-group not implemented"))

(defun survey-remove-group (widget)
  (declare (ignore widget))
  (do-information "survey-remove-group not implemented"))

(defun survey-edit-group (widget)
  (declare (ignore widget))
  (do-information "survey-edit-group not implemented"))

(defun survey-comment-group (widget)
  (declare (ignore widget))
  (do-information "survey-comment-group not implemented"))

;; The fact that we're having to manually manage both the list
;; of groups and the list of group-summary-widget indicates that
;; we need to come up with a better way...
(defun survey-editor-add-group (survey-editor group)
  (with-slots (survey group-widgets) survey-editor
    (let ((session-survey (session-object survey)))
      (if (member group (survey-groups session-survey))
	(do-information (cl-who:escape-string
			 (format nil "group ~s already in survey" group)))
	(progn
	  ;; Undo doesn't work when adding a group.
	  ;; Users will just have to remove groups if they add them
	  ;; when they don't want to.
	  (setf (survey-groups session-survey)
		(append (survey-groups session-survey) (list group)))
	  ;; In addition to updating the session survey above, update
	  ;; the "real" survey so that discarding edits doesn't orphan
	  ;; the new group.
	  (unless (eq session-survey survey)
	    (setf (survey-groups survey)
		  (append (survey-groups survey) (list group))))
	  (update-group-summary-widgets survey-editor)
          (record-event :survey-changed survey))))))

(defun survey-editor-handle-drop (survey-editor item)
  (let* ((id (parse-trailing-integer item))
	 (obj (get-model 'survey-group id)))
    (check-type obj survey-group)
    (survey-editor-add-group survey-editor obj)))

(defun survey-editor-remove-group (survey-editor group)
  (save-undo-state survey-editor)
  (with-slots (survey group-widgets) survey-editor
    (let ((session-survey (session-object survey)))
      (setf (survey-groups session-survey)
	    (remove group (survey-groups session-survey)))
      (update-group-summary-widgets survey-editor))))

(defun/cc get-group-from-user ()
  (flet ((get-group-helper (grid item)
	   (format *trace-output* "~&picked group: ~a~%" item)
	   (answer grid item)))
    (let ((datagrid (make-instance 'datagrid
				   :data-class 'survey-group
				   :view 'survey-group-simple-view
				   :allow-drilldown-p t
				   :on-drilldown
				   (cons :select #'get-group-helper))))
      (do-dialog "Select page" datagrid))))

(defun/cc get-question-from-user (group &optional (prompt "Select question"))
  (flet ((get-question-helper (grid item)
	   (format *trace-output* "~&picked question: ~a~%" item)
	   (answer grid item)))
    (let ((datagrid (make-instance 'datagrid
				   :data-class 'question
				   :on-query (list :filter-fn
						   #'(lambda (q)
						       (not (eq (parent q)
								group))))
				   :view 'question-table-view
				   :allow-drilldown-p t
				   :on-drilldown
				   (cons :select #'get-question-helper))))
      (do-dialog prompt datagrid))))

;; It's a miracle this works.
(defun survey-editor-add-existing-group (survey-editor)
  (let ((comp (make-instance 'composite :dom-id "add-existing-group")))
    (flet ((add-group-helper (grid item)
	     (declare (ignore grid))
	     ;;(format *trace-output* "~&add existing group: ~a~%" item)
	     (answer comp)
	     (survey-editor-add-group survey-editor item)))
      (let ((datagrid 
	     (make-instance 'datagrid
			    :data-class 'survey-group
			    :on-query (list :filter-fn (add-group-filter))
			    :view 'survey-group-simple-view
			    :allow-drilldown-p t
			    :on-drilldown
			    (cons :add  #'add-group-helper))))
	(setf (pagination-items-per-page (dataseq-pagination-widget datagrid)) 10)
	(setf (composite-widgets comp)
	      (list datagrid
		    (make-widget
		     (lambda ()
		       (with-html-form (:post (f* (answer comp)))
			 (render-translated-button "cancel")))))))
      (do-dialog "Add existing page" comp))))

(defun add-group-filter ()
  (let ((top-level (top-level-groups)))
    (lambda (group)
      (not (or (member group top-level)
	       (eq (owner group) (current-user)))))))

(defun get-legal-subgroups (group)
  "Returns all groups without parents that aren't top-level groups"
  (nreverse
   (filter-if #'parent
	      (set-difference (get-instances-by-class 'survey-group)
			      (cons group (top-level-groups))))))


(defun survey-editor-edit-acl (survey-editor)
  (with-slots (survey) survey-editor
    (let ((comp (make-instance 'composite)))
      (flet ((add-user-to-acl (grid user)
	       (declare (ignore grid))
	       (answer comp))
	     (get-data (w sort range &key countp)
	       (if countp
		 (length (survey-acl survey))
		 (when range
		   (subseq (survey-acl survey) (car range) (cdr range)))))
	     (process-form (&key username &allow-other-keys)
	       (when (get-user username)
		 (push (get-user username) (survey-acl survey))
		 (mark-dirty comp))
	       nil))
	(setf (composite-widgets comp)
	      (list
	       (make-widget
		(lambda (&rest args)
		  (declare (ignore args))
		;(render-object-view (survey-acl survey) 'user-brief-view)
		  (with-html
		    (if (survey-acl survey)
			(htm
			 (:ul
			  (dolist (user (survey-acl survey))
			    (htm (:li (str (username user))
				      (render-link
				       #'(lambda (&rest args)
					   (declare (ignore args))
					   (setf (survey-acl survey)
						 (remove (get-user user)
							 (survey-acl survey)))
					   (mark-dirty comp))
				       " [remove]"))))))
			(htm
			 (:p
			  "No other users have yet been given permission to edit this survey"))))
		  (with-html-form (:post #'process-form)
		    (with-html
		      (:label :for "username" "Username: ")
		      (render-text-input "username" "" :id "username")
		      (render-translated-button "Grant edit permission")))))
;; 	       (make-instance 'datalist
;; 			      :data-class 'user
;; 			      :on-query #'get-data)
	       (make-widget
		(lambda ()
		  (with-html-form (:post (lambda (&rest args)
					   (declare (ignore args))
					   (answer comp)))
		    (render-translated-button "Done"))))))
	(do-dialog "Manage Survey Editors" comp)))))

;;; Duplicating slots like this is rather crude, but I'm not sure
;;; what else to do.
(defclass proxy-survey ()
  ((name :accessor name :initarg :name :initform nil)
   (description :accessor description :initarg :description
		:initform nil)
   (help :accessor help :initarg :help :initform nil)
   (groups :accessor survey-groups :initarg :groups :initform nil)
   (published :accessor published-p :initarg :published :initform nil)
   (sticky :accessor sticky-p :initarg :sticky :initform nil)
   (origin :accessor origin :initarg :origin :initform nil)
   (diary-p :accessor diary-p :initarg :diary-p :initform nil
	    :documentation "Is the whole survey a diary entry (single group)?")
   (diary-question :accessor diary-question 
		   :initarg :diary-question
		   :initform nil)
   (diary-description :accessor diary-description 
		      :initarg :diary-description
		      :initform nil)))


(defun make-proxy-survey (survey)
  (check-type survey (or survey proxy-survey))
  (make-instance 'proxy-survey
		 :name (copy-seq (name survey))
		 :description (copy-seq (description survey))
		 :help (copy-seq (help survey))
		 :groups (copy-list (survey-groups survey))
		 :published (published-p survey)
                 :sticky (sticky-p survey)
                 :origin (or (origin survey) "inherit")
		 :diary-p (diary-p survey)
		 :diary-question (diary-question survey)
		 :diary-description (diary-description survey)))

(defwidget survey-editor-widget (widget undo-redo-mixin)
  ((survey :accessor survey-editor-survey :initarg :survey)
   (edited-p :accessor survey-editor-edited-p :initarg :edited-p
	     :initform nil)
   (ui-state :accessor survey-editor-ui-state :initarg :ui-state
	     :initform :view)
   (group-widgets :accessor survey-editor-group-widgets
		  :initarg :group-widgets :initform nil)))


(defun make-survey-editor-widget (survey action-pane)
  (make-instance 'survey-editor-widget
		 :dom-id
		 (format nil "survey_~d" (mid survey))
		 :survey survey
		 :propagate-dirty (list action-pane)))

(defmethod mark-dirty ((w survey-editor-widget) &key weblocks::putp &allow-other-keys)
  (declare (ignore weblocks::putp))
  (call-next-method w :putp t))

(defun make-group-summary-widgets (survey-editor)
  (let ((widgets nil)
	(session-survey (session-object (survey-editor-survey survey-editor))))
    (dolist (g (survey-groups session-survey) (nreverse widgets))
      (let ((gw (make-group-summary-widget g)))
	(setf (widget-parent gw) survey-editor)
	(push gw widgets)))))

(defmethod initialize-instance :after ((w survey-editor-widget) &rest initargs)
  (declare (ignore initargs))
  (with-slots (survey group-widgets) w
    (setf (session-object survey) (make-proxy-survey survey))
    (setf group-widgets (make-group-summary-widgets w))))

(defun survey-editor-update-selection (survey-editor id)
  (dolist (gw (survey-editor-group-widgets survey-editor))
    (if (string= id (dom-id gw))
      (setf (group-summary-selected-p gw) t)
      (setf (group-summary-selected-p gw) nil))))

(defun survey-editor-reorder-groups (survey-editor sortable-string)
  (let ((id-list (parse-sortable-output sortable-string))
	(session-survey (session-object (survey-editor-survey survey-editor))))
    (save-undo-state survey-editor)
    ;; update group list
    (setf (survey-groups session-survey) (items-from-id-list id-list 'survey-group))
    ;; update list of group widgets
    (update-group-summary-widgets survey-editor)))

(defun survey-editor-dirty-p (survey-editor)
  (with-slots (survey) survey-editor
    (let ((session-survey (session-object survey)))
      (not (and (string= (name session-survey) (name survey))
		(string= (description session-survey) (description survey))
		(string= (help session-survey) (help survey))
		(tree-equal (survey-groups session-survey)
			    (survey-groups survey)))))))

(defun survey-editor-discard-changes (survey-editor)
  (with-slots (survey group-widgets) survey-editor
    (dolist (gw group-widgets)
      (clear-session-object (group-summary-group gw)))
    (clear-session-object survey)
    ;; we're still in the editor, so we still need a proxy copy
    (setf (session-object survey) (make-proxy-survey survey))
    (clear-undo-state survey-editor)
    (update-group-summary-widgets survey-editor)))

(defun all-equal (a b &rest accessors)
  (or (eq a b)
      (every (lambda (x) (equal (funcall x a) (funcall x b)))
             accessors)))

(defun survey-editor-commit-changes (survey-editor)
  ;; Maybe see if any changes were made to the real survey in the
  ;; meantime and do appropriate conflict resolution?
  (with-slots (survey) survey-editor
    (let* ((session-survey (session-object survey))
	   (groups nil))
      ;; If the user created any new groups, they will be proxy groups.
      ;; Turn any proxy groups into persistent objects.
      (dolist (g (survey-groups session-survey))
	(if (typep g 'survey-group)
	  (push g groups)
	  (push (make-survey-group-from-proxy g) groups)))
      (setq groups (nreverse groups))
      (clear-session-object survey)
      (mapc #'clear-session-object (survey-groups survey))
      (clear-undo-state survey-editor)
      (unless (all-equal survey session-survey
                         #'name #'published-p #'sticky-p #'origin
                         #'description #'help #'survey-groups)
        (record-event :survey-changed survey)
        (when (and (published-p session-survey)
                   (not (published-p survey)))
          (record-event :survey-published survey)))                   
      (ensure-transaction ()
	(setf (name survey) (name session-survey))
	(setf (published-p survey) (published-p session-survey))
	(setf (sticky-p survey) (sticky-p session-survey))
	(setf (origin survey) (origin session-survey))
	(setf (description survey) (description session-survey))
	(setf (help survey) (help session-survey))
	(setf (survey-groups survey) groups)))))

(defun update-group-summary-widgets (survey-editor)
  (let* ((session-survey (session-object (survey-editor-survey survey-editor)))
	 (group-list (survey-groups session-survey)))
    (with-slots (group-widgets) survey-editor
      (if (= (length group-list) (length group-widgets))
	;; simple re-ordering
	(setf group-widgets
	      (re-order-list group-widgets group-list #'group-summary-group))
	;; a group was added/removed, so make new widgets
	(setf group-widgets (make-group-summary-widgets survey-editor))))))

(defmethod save-undo-state ((survey-editor survey-editor-widget))
  (with-slots (survey undo-history) survey-editor
    (push (make-proxy-survey (session-object survey))
	  undo-history)))

(defmethod undo ((survey-editor survey-editor-widget))
  (with-slots (undo-history redo-history survey) survey-editor
    (unless (null undo-history)
      (push (session-object survey) redo-history)
      (setf (session-object survey) (pop undo-history))
      (update-group-summary-widgets survey-editor))))

(defmethod redo ((survey-editor survey-editor-widget))
  (with-slots (redo-history undo-history survey) survey-editor
    (unless (null redo-history)
      (push (session-object survey) undo-history)
      (setf (session-object survey) (pop redo-history))
      (update-group-summary-widgets survey-editor))))

(defun render-survey-properties (w)
  (with-html
    (:div :id "survey-properties"
      (:p "This survey is " (if (published-p (survey-editor-survey w))
			      (str "published. ")
			      (str "not published. "))
	  (render-link #'(lambda (&rest args)
			   (declare (ignore args))
			   (with-slots (published) (survey-editor-survey w)
			     (setf published (not published))
			     (if published
			       (record-event :survey-published
					     (survey-editor-survey w))))
			   (mark-dirty w))
		       (if (published-p (survey-editor-survey w))
			 "Unpublish this survey."
			 "Publish this survey."))))))

(defun render-diary-properties (w)
  (with-html
    (:ul
     (:li "Key field: "
	  (str (cl-who:escape-string
		(format nil "~s" (diary-question (survey-editor-survey w)))))
	  (render-link (lambda/cc (&rest args)
			 (declare (ignore args))
			 (setf (diary-question (survey-editor-survey w))
			       (get-question-from-user (car
							(survey-groups
							 (survey-editor-survey w)))))
			 (mark-dirty w))
		       "[change]"))
     (:li "Description: "
	  (str (cl-who:escape-string
		(format nil "~s"
			(diary-description (survey-editor-survey w)))))
	  (render-link (lambda/cc (&rest args)
			 (declare (ignore args))
			 (setf (diary-description (survey-editor-survey w))
			       (get-question-from-user (car
							(survey-groups
							 (survey-editor-survey w)))))
			 (mark-dirty w))
		       "[change]")))))

(defun embed-action-code (action-code id)
  (with-html
    (:input :id id :type "hidden" :value action-code)))

(defun get-diary-questions (survey)
  (flatten (mapcar #'group-questions (survey-groups survey))))

(defmethod print-view-field-value ((value question) presentation
				   field view widget obj &rest args)
  (declare (ignore presentation field view obj args widget))
  (question-name value))

(defview survey-properties-data-view (:type data :caption "")
  description
  help
  (published :present-as predicate))

(defview survey-properties-editor-data-view (:inherit-from
                                             'survey-properties-data-view
                                             :type data :caption "")
  (sticky :present-as predicate)
  (origin :present-as (text :format-string "~/REGISTRY:FORMAT-SURVEY-ORIGIN/")))

(defview survey-properties-form-view (:type form :persistp nil :caption "")
  (name :present-as (textarea :cols 80 :rows 1))
  (description :present-as textarea)
  (help :present-as textarea)
  (published :present-as checkbox))

(defview survey-properties-editor-form-view (:inherit-from
                                             'survey-properties-form-view
                                             :type form :persistp nil :caption "")
  (sticky :present-as checkbox)
  (origin :present-as (dropdown :choices (lambda (x)
                                           (declare (ignore x))
                                           (survey-origin-choices)))
          :requiredp t))

(defview diary-properties-data-view (:inherit-from
				     'survey-properties-data-view
				     :type data
				     :caption "")
  diary-question
  diary-description)

(defview diary-properties-form-view (:inherit-from
				     'survey-properties-form-view
				     :type form
				     :caption ""
				     :persistp nil)
  (diary-question :present-as (dropdown :choices 'get-diary-questions
					:label-key #'question-name)
		  :parse-as (mid)
		  :reader (compose #'mid #'diary-question))
  (diary-description :present-as (dropdown :choices 'get-diary-questions
					   :label-key #'question-name)
		     :parse-as (mid)
		     :reader (compose #'mid #'diary-description)))

(defwidget survey-properties-dataform (dataform)
  ((survey-widget :initarg :survey-widget :accessor survey-widget)
   (was-published-p :initarg :was-published-p
                    :initform nil
                    :accessor was-published-p)))

(defmethod dataform-submit-action ((dataform survey-properties-dataform) data
				   &rest args)
  (declare (ignore args data))
  ;; We have defined form views with :persistp nil.  This makes weblocks
  ;; not try to call persist-object on the dataform's data object.
  ;; This is good, because the data object in this case will be the
  ;; session-object for the survey being edited.
  ;;
  ;; Before this data object gets updated, we want to save the current state.
  ;; Afterwards, we update the session-object for the survey.
  (let* ((survey-widget (survey-widget dataform)))
    (save-undo-state survey-widget)
    (call-next-method)
    (let ((data (dataform-data dataform)))
      (setf (session-object (survey-editor-survey survey-widget)) data)
      (when (typep data 'survey)
        (record-event :survey-changed data)
        (when (and (published-p data) (not (was-published-p dataform)))
          (record-event :survey-published data))))))

(defmethod render-widget-body ((w survey-editor-widget) &rest args)
  (declare (ignore args))
  (embed-action-code (make-action (lambda (&key id &allow-other-keys)
				    (survey-editor-update-selection w id)))
		     "update-selection-action-code")
  (embed-action-code (make-action (lambda (&key list &allow-other-keys)
				    (survey-editor-reorder-groups w list)))
		     "reorder-action-code")
  (embed-action-code (make-action (lambda (&key item &allow-other-keys)
				    (survey-editor-handle-drop w item)))
		     "accept-drop-action-code")
  (let ((session-survey (session-object (survey-editor-survey w))))
    (with-slots (ui-state group-widgets) w
      (with-html
	(:h1 :class "survey-name"
	     "Editing Survey: " (str (name session-survey)))
	;;	(render-survey-properties w)
	(if (diary-p (survey-editor-survey w))
	  (render-widget (make-instance 'survey-properties-dataform
					:survey-widget w
					:data-view 'diary-properties-data-view
					:form-view 'diary-properties-form-view
					:data session-survey
                                        :was-published-p
                                        (published-p session-survey)))
	  (render-widget (make-instance 'survey-properties-dataform
					:survey-widget w
					:data-view
                                        (if (is-editor-p)
                                            'survey-properties-editor-data-view
                                            'survey-properties-data-view)
					:form-view
                                        (if (is-editor-p)
                                            'survey-properties-editor-form-view
                                            'survey-properties-form-view)
					:data session-survey)))
	(:hr)
	(:div :id (format nil "~a-sortable" (dom-id w))
	      (dolist (gw group-widgets)
		(render-widget gw)))))))

;; Survey Actions for Sidebar Widget

(defun survey-action-undo (w)
  (unless (null (undo-history w))
    (render-link (f* (undo w)) "Undo")))

(defun survey-action-redo (w)
  (unless (null (redo-history w))
    (render-link (f* (redo w)) "Redo")))

(defun survey-action-add-existing-page (w)
  (render-link (f* (survey-editor-add-existing-group w))
	       "Add Existing Page"))

(defun survey-action-add-new-page (w)
  (render-link (f* (do-dialog ""
		     (make-quickform 'survey-group-new-view
				     :data-class-name 'survey-group
				     :on-success
				     #'(lambda (qform g)
					 (declare (ignore qform))
					 (setq g (persist-object
						  *default-store*
						  g))
					 (setf (owner g) (current-user))
					 (survey-editor-add-group w g)))))
	       "Add New Page"))

(defun survey-action-save-changes (w)
  (when (or (undo-history w) (redo-history w))
    (render-link (f* (survey-editor-commit-changes w))
		 "Save Changes")))

(defun survey-action-revert-changes (w)
  (when (or (undo-history w) (redo-history w))
    (render-link (f* (survey-editor-discard-changes w))
		 "Revert Changes")))

(defun survey-action-manage-editors (w)
  (render-link (f* (survey-editor-edit-acl w))
	       "Manage Editors"))

(defun survey-action-diary-conversion (w)
  (if (diary-p (survey-editor-survey w))
      (render-link (f* (setf (diary-p (survey-editor-survey w)) nil)
		       (mark-dirty w))
		   "Convert to simple survey")
      (let ((group (car (survey-groups (survey-editor-survey w)))))
	(render-link (lambda/cc (&rest args)
		       (declare (ignore args))
		       (let* ((diary-question (get-question-from-user group "Select diary question")))
			 (when diary-question
			   (setf (diary-question (survey-editor-survey w))
				 diary-question
				 (diary-p (survey-editor-survey w))
				 t)
			   (mark-dirty w))))
		     "Convert to diary"))))


;;
;; GROUP SUMMARY WIDGET
;;

(defwidget group-summary-widget (widget)
  ((group :accessor group-summary-group :initarg :group :initform nil)
   (selected-p :reader group-summary-selected-p :initarg :selected-p
	       :initform nil)))

(defmethod (setf group-summary-selected-p) (flag (gw group-summary-widget))
  (setf (slot-value gw 'selected-p) flag)
  (if flag
    (setf (dom-class gw) "selected")
    (setf (dom-class gw) nil)))

(defun make-group-summary-widget (group)
  (make-instance 'group-summary-widget
		 :group group
		 :dom-id (format nil "group_~d" (mid group))))

(defmethod render-widget-body ((w group-summary-widget) &rest args)
  (declare (ignore args))
  (with-slots (group) w
    (let ((session-group (session-object group)))
      (with-html
	(:div :class "group-summary-name"
	    (:span :class "group-summary-text"
		   (str (group-name session-group))
		   (when (is-admin-p)
		     (str (format nil " (~a)" (mid group)))))
	    (:span :class "group-summary-actions"
		   (when (editable-by-user-p group)
		     (render-image-link (lambda (&rest args)
					  (declare (ignore args))
					  (let ((tabnav (find-workspace-widget w 'editor-tabnav)))
					    (do-pane tabnav group (make-survey-editor-view
								   tabnav group)
						     (survey-object-edit-tokens group))))
					"/pub/images/icons/file.png"
					:title "Edit Page")
		   (render-image-link #'(lambda (&rest args)
					  (declare (ignore args))
					  (survey-editor-remove-group (widget-parent w) group))
				      "/pub/images/icons/action_delete.png"
				      :title "Remove Page"))))
		   
	(:ul
	 (dolist (q (group-questions session-group))
	   (setq q (session-object q))
	   (htm
	    (:li (str (question-prompt q)))))))
      )))
;;       (when (editable-by-user-p group)
;; 	(render-link (lambda (&rest args)
;; 		       (declare (ignore args))
;; 		       (let ((tabnav (find-workspace-widget w 'editor-tabnav)))
;; 			 (do-pane tabnav group (make-survey-editor-view
;; 						tabnav group)
;; 				  (survey-object-edit-tokens group))))
;; 		     "[edit group]"))
;;       (render-link #'(lambda (&rest args)
;; 		       (declare (ignore args))
;; 		       (survey-editor-remove-group (widget-parent w) group))
;; 		   "[remove group from survey]"))))
  
;; ======================================
;;  Group Objects and Editing View
;; ======================================

;; NOTE: How do we lookup the URL location of a given widget
;;  do we have a global map for URL mapping?  Can it be relative
;;  to a object?.  This is a hack for now.

(defwidget group-editor-widget (widget undo-redo-mixin)
  ((group :accessor group-editor-group :initarg :group
	  :initform nil)
   (ui-state :accessor group-editor-ui-state :initarg :ui-state :initform :view)
   (show-comments-p :accessor group-editor-show-comments-p
		    :initarg :show-comments-p :initform nil)
   (parent-survey :accessor group-editor-parent-survey 
		  :initarg :parent-survey)
   (question-widgets :accessor group-editor-question-widgets
		     :initarg :question-widgets :initform nil)))

(defmethod mark-dirty ((w group-editor-widget) &key weblocks::putp &allow-other-keys)
  (declare (ignore weblocks::putp))
  (call-next-method w :putp t))

(defmethod survey-object-edit-url ((group survey-group))
  (format nil "/dashboard/create/group/~A" (mid group)))

(defmethod survey-object-edit-tokens ((group survey-group))
  (list "group" (format nil "~A" (mid group))))

(defmethod make-survey-editor-view (nav (group survey-group))
  (if (not (editable-by-user-p group))
    "You don't have permission to edit this group."
    (let* ((gw (make-group-editor-widget 
		group
		(find-workspace-widget nav 'survey-editor-actions))))
      (update-search-actions nav '(("Add Question" group-add-question)
				   ("Delete Question" group-delete-question)
				   ("View Group Comments" group-view-comments)
				   ("View All Comments" group-view-all-comments)
				   ("Delete this Page" group-delete-group)))
      (setf (widget-parent gw) nav)
      gw)))

(defun find-group-editor-widget (self)
  "Given a widget SELF (probably the action widget), return the
   the active group-editor-widget or NIL if a group-editor-widget
   is not active."
  (let* ((nav (find-workspace-widget self 'editor-tabnav))
	 (pane (current-dynamic-pane nav))
	 (widget (dynamic-pane-widget pane)))
    (if (typep widget 'group-editor-widget)
      widget
      nil)))

(defun all-group-editors (self)
  (let* ((nav (find-workspace-widget self 'editor-tabnav))
	 (panes (dynamic-panes nav))
	 (widgets nil))
    (dolist (pane panes widgets)
      (let ((widget (dynamic-pane-widget pane)))
	(when (typep widget 'group-editor-widget)
	  (push widget widgets))))))

;; sidebar actions for group editor

(defun group-add-question (widget)
  (let ((gw (find-group-editor-widget widget)))
    (if gw
      (let* ((group (group-editor-group gw))
	     (q (make-instance 'question
			       :parent group
			       :data-type nil
			       :name "New question"
			       :prompt "New question prompt"
			       :help "What is this question about?"))
	     (qw (make-question-summary-widget q)))
	(setf (widget-parent qw) gw)
	(setf (group-questions group) (append (group-questions group) (list q)))
	(setf (group-editor-question-widgets gw)
	      (append (group-editor-question-widgets gw) (list qw)))
	(format *trace-output* "~&added question ~s, widget ~s~%" q qw))
      (format *trace-output* "~&a group widget is not active~%"))))

(defun group-delete-question (widget)
  (declare (ignore widget))
  (do-information "group-delete-question not implemented"))

(defun group-view-comments (widget)
  (let ((gw (find-group-editor-widget widget)))
    (if gw
      (setf (group-editor-show-comments-p gw) t)
      (format *trace-output* "~&a group widget is not active~%"))))

(defun group-view-all-comments (widget)
  (let ((gw (find-group-editor-widget widget)))
    (if gw
      (mapc #'(lambda (w) (setf (question-summary-show-comments-p w) t))
	    (group-editor-question-widgets gw))
      (do-information "No group editor appears to be active"))))

(defun group-delete-group (widget)
  (declare (ignore widget))
  (do-information "delete this group unimplemented"))

(defun group-parents (group)
  "Search all surveys, returning a list of those that contain GROUP."
  (let (parents)
    (dolist (s (get-instances-by-class 'survey) parents)
      (when (member group (survey-groups s))
	(push s parents)))))

;;
;; Group ACL
;;

(defun group-editor-edit-acl (group-editor)
  (with-slots (group) group-editor
    (let ((comp (make-instance 'composite)))
      (flet ((add-user-to-acl (grid user)
	       (declare (ignore grid))
	       (answer comp))
	     (get-data (w sort range &key countp)
	       (if countp
		 (length (group-acl group))
		 (when range
		   (subseq (group-acl group) (car range) (cdr range)))))
	     (process-form (&key username &allow-other-keys)
	       (when (get-user username)
		 (push (get-user username) (group-acl group))
		 (mark-dirty comp))
	       nil))
	(setf (composite-widgets comp)
	      (list
	       (make-widget
		(lambda (&rest args)
		  (declare (ignore args))
		   ;(render-object-view (survey-acl survey) 'user-brief-view)
		   (with-html
		     (if (group-acl group)
		       (htm
			(:ul
			 (dolist (user (group-acl group))
			   (htm (:li (str (username user))
				     (render-link
				      #'(lambda (&rest args)
					  (declare (ignore args))
					  (setf (group-acl group)
						(remove (get-user user)
							(group-acl group)))
					  (mark-dirty comp))
				      " [remove]"))))))
		       (htm
			(:p
			 "No other users have permission to edit this group"))))
		   (with-html-form (:post #'process-form)
		     (with-html
		       (:label :for "username" "Username: ")
		       (render-text-input "username" "" :id "username")
		       (render-translated-button "Grant edit permission")))))
;; 	       (make-instance 'datalist
;; 			      :data-class 'user
;; 			      :on-query #'get-data)
		(make-widget
		 (lambda ()
		   (with-html-form (:post (lambda (&rest args)
					    (declare (ignore args))
					    (answer comp)))
		     (render-translated-button "Done"))))))
	(do-dialog "Manage Survey Editors" comp)))))

;;
;; Proxy group objects
;;

(defclass proxy-group ()
  ((id :initarg :id :initform (gensym)
       :documentation "Weblocks magic slot name.")
   (name :accessor group-name :initarg :name :initform "New page")
   (advice :accessor group-advice :initarg :advice :initform "")
   (questions :accessor group-questions :initarg :questions :initform nil)))

(defun make-proxy-group (group)
  (check-type group (or proxy-group survey-group))
  (make-instance 'proxy-group
		 :name (copy-seq (group-name group))
		 :advice (copy-seq (group-advice group))
		 :questions (copy-list (group-questions group))))

(defun make-survey-group-from-proxy (proxy-group)
  (check-type proxy-group proxy-group)
  (make-instance 'survey-group
		 :name (copy-seq (group-name proxy-group))
		 :advice (copy-seq (group-advice proxy-group))
		 :questions (copy-list (group-questions proxy-group))))

(defun group-editor-add-question (group-editor question)
  (with-slots (group question-widgets) group-editor
    (let ((session-group (session-object group)))
      ;; Undo doesn't work for this.
      (setf (group-questions session-group)
	    (append (group-questions session-group) (list question)))
      (unless (eq group session-group)
	(setf (group-questions group)
	      (append (group-questions group) (list question))))
      (setf (parent question) group)
      (update-question-summary-widgets group-editor)
      (record-event :group-changed group))))

;; Can't really undo this...
(defun group-editor-delete-question (group-editor question)
  ;; Seek and destroy the question in...

  ;; the session object
  (let* ((session-group (session-object (group-editor-group group-editor))))
    (setf (group-questions session-group)
	  (remove question (group-questions session-group))))

  ;; any proxy group objects on the group-editor-widget's undo/redo stacks
  (dolist (thing (undo-history group-editor))
    (typecase thing
      (proxy-group (setf (group-questions thing)
			 (remove question (group-questions thing))))))
  (dolist (thing (redo-history group-editor))
    (typecase thing
      (proxy-group (setf (group-questions thing)
			 (remove question (group-questions thing))))))

  ;; the scratchpad
  (let ((scratchpad (find-workspace-widget group-editor
					   'scratchpad)))
    (setf (scratchpad-id-list scratchpad)
	  (remove (mid question) (scratchpad-id-list scratchpad))))

  ;; the real survey-group object
  (remove-question (group-editor-group group-editor) question)
  (update-question-summary-widgets group-editor)
  (drop-instance question))

(defun/cc do-edit-question-dialog (w question)
  (do-dialog "Edit question" (make-question-editor-widget question))
  (update-question-summary-widgets (widget-parent w)))

(defun make-group-editor-widget (group action-pane)
  (make-instance 'group-editor-widget
		 :dom-id
		 (format nil "group_~d"(mid group))
		 :group group
		 :propagate-dirty (list action-pane)))

(defun make-question-summary-widgets (group-editor)
  (let ((widgets nil)
	(session-group (session-object (group-editor-group group-editor))))
  (dolist (q (group-questions session-group) (nreverse widgets))
    (let ((qw (make-question-summary-widget q)))
      (setf (widget-parent qw) group-editor)
      (push qw widgets)))))

(defmethod initialize-instance :after ((w group-editor-widget) &rest initargs)
  (declare (ignore initargs))
  (with-slots (group question-widgets) w
    (setf (session-object group) (make-proxy-group group))
    (setf question-widgets (make-question-summary-widgets w))
    (dolist (qw question-widgets)
      (if (question-summary-selected-p qw)
	(setf (dom-class qw) "selected")
	(setf (dom-class qw) nil)))))

(defun parse-trailing-integer (string)
  (loop for i from (1- (length string)) downto 0
	when (not (digit-char-p (char string i)))
	return (parse-integer string :start (1+ i))))

(defun all-survey-editors (some-widget)
  (let* ((nav (find-workspace-widget some-widget 'editor-tabnav))
	 (panes (dynamic-panes nav))
	 (widgets nil))
    (dolist (pair panes widgets)
      (let ((widget (cdr pair)))
	(when (typep widget 'survey-editor-widget)
	  (push widget widgets))))))

(defmethod editor-dirty-p ((editor undo-redo-mixin))
  (or (undo-history editor)
      (redo-history editor)))

;; defun/cc so that do-confirmation will work
(defun/cc group-editor-handle-drop (group-editor item)
  (let* ((id (parse-trailing-integer item))
	 (question (get-model 'question id)))
    (check-type question question)
    (format *trace-output* "~&got dropped thing ~s~%" question)
    ;; This is problematic.
    ;; Moving a question is a rather special operation.
    ;; We have to keep object identity constant because questions have
    ;; associated answers.  Also, as an invariant, questions belong to
    ;; one and only one group.  It's therefore hard to do this in an
    ;; undoable way.
    ;; We therefore impose some fairly user-hostile conditions:
    ;;  * the destination group must be in a non-edited state
    ;;  * the source group must be in a non-edited state
    ;;  * the operation of moving a question cannot be undone.

    (let* ((src-group (parent question))
	   (src-group-editor (find src-group (all-group-editors group-editor)
				   :key #'group-editor-group))
	   (dst-group (group-editor-group group-editor))
	   (dst-group-editor group-editor))

      (cond
	((and src-group-editor
	      (editor-dirty-p src-group-editor))
	 (do-information "Questions can't be moved from a group that
has unsaved changes.  Please save or discard your changes and try again."))
	((and dst-group-editor
	      (editor-dirty-p dst-group-editor))
	 (do-information "Questions can't be moved to a group that
has unsaved changes.  Please save or discard your changes and try again."))
	((member question (group-questions dst-group))
	 (do-information "That question is already in the group."))
	(t
	 (let ((msg (format nil "Really move question ~s (~d) from group ~s (~d) to group ~s (~d)? (This action cannot be undone.)"
			    (question-name question) (mid question)
			    (group-name src-group) (mid src-group)
			    (group-name dst-group) (mid dst-group)))
	       (result nil))
	   (setq result (do-confirmation (cl-who:escape-string msg)))
	   (format *trace-output* "~&you said ~s~%" result)
	   (when (eq result :ok)
	     (move-questions src-group (list question) dst-group)

	     ;; OK, at this point we've (carefully) updated the actual
	     ;; persistent objects.  Now, we've got to update the user
	     ;; interface to reflect this change.

	     ;; 1. Remove question from scratchpad.
	     (let ((scratchpad (find-workspace-widget group-editor
						      'scratchpad)))
	       (setf (scratchpad-id-list scratchpad)
		     (remove id (scratchpad-id-list scratchpad))))

	     ;; 2. Update the destination group editor.  This mainly
	     ;; involves creating a new set of question summary widgets.

	     ;; This is horrid, but the undo scheme of having a
	     ;; session-object is getting in the way.  We know the
	     ;; group-editor is not dirty, so we can get away with this.
	     (setf (session-object dst-group) (make-proxy-group dst-group))
	     (setf (group-editor-question-widgets dst-group-editor)
		   (make-question-summary-widgets dst-group-editor))


	     ;; 3. Update the source group editor, if it's open.
	     (when src-group-editor
	       (setf (session-object src-group) (make-proxy-group src-group))
	       (setf (group-editor-question-widgets src-group-editor)
		     (make-question-summary-widgets src-group-editor)))


	     ;; 4. Find any open survey editor widgets, and if any of
	     ;; them contain group-summary-widgets for either of the
	     ;; two groups that we just updated, fix them up.

	     ;; We also need to get other open editors to update their
	     ;; displays somehow.

	     ;; Now, if it's open, we need to find the group-editor
	     ;; containing original question so that we can update the
	     ;; question widgets there as well.  For simplicity, just
	     ;; update all open group editors.
	     (let ((editors (all-survey-editors group-editor)))
	       (format *trace-output* "~&editors = ~s~%" editors)
	       (dolist (e editors)
		 (update-group-summary-widgets e))))))))))

(defun group-editor-dirty-p (group-editor)
  (with-slots (group proxy-group) group-editor
    (not (and (string= (group-name group) (group-name proxy-group))
	      (string= (group-advice group) (group-advice proxy-group))
	      (tree-equal (group-questions group)
			  (group-questions proxy-group))))))

(defun make-question-from-proxy (proxy-question)
  (make-instance 'question
		 :name (question-name proxy-question)
		 :prompt (question-prompt proxy-question)
		 :help (question-help proxy-question)
		 :data-help (question-data-help proxy-question)
		 :data-type (question-data-type proxy-question)
		 :data-subtype (question-data-subtype proxy-question)
		 :view-type (question-view-type proxy-question)
		 :choices (question-choices proxy-question)))

(defun group-editor-discard-changes (group-editor)
  ;; Clear out any edits to questions that are members of this
  ;; group.
  (with-slots (group question-widgets) group-editor
    (dolist (qw question-widgets)
      (clear-session-object (question-summary-question qw)))
    (clear-session-object group)
    (setf (session-object group) (make-proxy-group group))
    (clear-undo-state group-editor)
    ;; Discard changes to any questions in the group at this point
    ;; also?
    (update-question-summary-widgets group-editor)))

(defun group-editor-commit-changes (group-editor)
  (with-slots (group question-widgets) group-editor
    (let ((session-group (session-object group))
	  (questions nil))
      (dolist (qw question-widgets)
	(let ((q (session-object (question-summary-question qw))))
	  ;; This is a bit tricky.
	  (cond ((typep q 'question)
		 ;; The question was not edited.
		 (push q questions))
		(t
		 ;; The question was edited.  The question slot of the
		 ;; question-summary-widget is a persistent object, so
		 ;; pick up the edits from the proxy.
		 (let ((persistent-q (question-summary-question qw)))
		   (ensure-transaction ()
		     (setf (question-prompt persistent-q) (question-prompt q))
		     (setf (question-help persistent-q)
			   (question-help q)))
		   (record-event :question-changed persistent-q)
		   (push persistent-q questions))))))
      (setq questions (nreverse questions))
      (clear-session-object group)
      (mapc #'clear-session-object questions)
      (clear-undo-state group-editor)
      (ensure-transaction ()
	(setf (group-name group) (group-name session-group))
	(setf (group-advice group) (group-advice session-group))
	(setf (group-questions group) questions))
      (when (typep group 'survey-group)
        (record-event :group-changed group)))))

(defun update-question-summary-widgets (group-editor)
  (let* ((session-group (session-object (group-editor-group group-editor)))
	 (question-list (group-questions session-group)))
    (with-slots (question-widgets) group-editor
      (if (= (length question-list) (length question-widgets))
	;; simple re-ordering
	(setf question-widgets (re-order-list question-widgets question-list
					      #'question-summary-question))
	;; a question was added/removed, so make new widgets
	(setf question-widgets
	      (make-question-summary-widgets group-editor))))))

(defmethod save-undo-state ((group-editor group-editor-widget))
  (with-slots (group undo-history) group-editor
    (push (make-proxy-group (session-object group))
	  undo-history)))

(defmethod undo ((group-editor group-editor-widget))
  (with-slots (undo-history redo-history group) group-editor
    (unless (null undo-history)
      (push (session-object group) redo-history)
      (setf (session-object group) (pop undo-history))
      (update-question-summary-widgets group-editor))))

(defmethod redo ((group-editor group-editor-widget))
  (with-slots (redo-history undo-history group) group-editor
    (unless (null redo-history)
      (push (session-object group) undo-history)
      (setf (session-object group) (pop redo-history))
      (update-question-summary-widgets group-editor))))

(defmethod render-widget-body ((w group-editor-widget) &rest args)
  (declare (ignore args))
  (embed-action-code (make-action (lambda (&key item &allow-other-keys)
				    (group-editor-handle-drop w item)))
		     "accept-drop-action-code")
  (embed-action-code (make-action (lambda (&key id &allow-other-keys)
				    (group-editor-update-selection w id)))
		     "update-selection-action-code")
  (embed-action-code (make-action (lambda (&key list &allow-other-keys)
				    (group-editor-reorder-questions w list)))
		     "reorder-group-action-code")
  (let ((session-group (session-object (group-editor-group w))))
    (with-slots (group question-widgets ui-state) w
      (with-html
	(:h1 "Editing group: " (str (group-name session-group)))
	#+show-useless-noise
	(when (is-admin-p)
	  (htm
	   (:pre (str (hunchentoot:escape-for-html
		       (format nil "~s" (group-rules group)))))))
	(:p :class "smallText"
	    (str (cl-who:escape-string
		  (format nil "This group is in the following surveys: ~s"
			  (mapcar #'name (group-parents group))))))
	(ecase ui-state
	  (:view
	   (htm
	    (:label (:b "Help text:")
	      (:blockquote (str (group-advice session-group))))))
	  (:edit
	   (render-group-header-editor w session-group)))
	(when (group-editor-show-comments-p w)
	  (render-object-comments group))
	(:div :id "group-editor-questions"
	      (:p "Drag questions to reorder them.")
	      (:div :id (format nil "~a-sortable" (dom-id w))
		    (dolist (qw question-widgets)
		      (render-widget qw))))))))
  ;;(send-script (format nil "groupEditCreateSortable('~a')" (dom-id w)))
;;  (with-html
;;    (when (or (undo-history w)
;;	      (redo-history w))
;;      (group-editor-question-widgets w)
;;      (render-link (lambda (&rest args)
;		     (declare (ignore args))
;;		   (group-editor-commit-changes w))
;		   "[save changes] ")

(defmethod render-object-comments ((g survey-group))
  (render-comments (get-comments g)))


(defun render-group-header-editor (w session-group)
  (flet ((process-form (&key group-name group-advice
			     cancel &allow-other-keys)
	   (when cancel
	     (setf (group-editor-ui-state w) :view)
	     (return-from process-form))
	   (when (or group-name group-advice)
	     (let ((pg (make-proxy-group session-group)))
	       (when group-name
		 (setf (group-name pg) group-name))
	       (when group-advice
		 (setf (group-advice pg) group-advice))
	       (save-undo-state w)
	       (setf (session-object (group-editor-group w)) pg)))
	   (setf (group-editor-ui-state w) :view)))
    (with-html-form (:post #'process-form)
      (with-html
	(:h1 (str #!"Group name"))
	(render-text-input "group-name" (group-name session-group))
	(:h2 (str #!"Group advice"))
	(render-textarea "group-advice" (group-advice session-group) 3 30)
	(render-translated-button "submit")
	(render-translated-button "cancel")))))


(defun do-preview-page (group)
  (do-dialog "Page Preview"
    (make-page-preview-widget group)))

;; Group Widget Actions


(defun group-action-modify-header (w)
  (when (eq (group-editor-ui-state w) :view)
    (render-link (f* (setf (group-editor-ui-state w) :edit))
		 "Modify Header")))

(defun group-action-preview-page (w)
  (when (and (group-editor-question-widgets w)
	     (group-editor-group w))
    (render-link (f* (do-preview-page (group-editor-group w)))
		 "Preview Page")))

(defun group-action-view-comments (w)
  (when (and (not (group-editor-show-comments-p w))
	     (has-comments-p (group-editor-group w)))
    (render-link (f* (setf (group-editor-show-comments-p w)
			   (not (group-editor-show-comments-p w))))
		 "View Comments")))
 
(defun group-action-clear-comments (w)
  (when (group-editor-show-comments-p w)
    (render-link (f* nil) "Clear Comments")))
 

(defun group-action-add-new-question (w)
  (render-link (f* (do-dialog "" (make-new-question-quickform w)))
	       "Add New Question"))


(defun make-new-question-quickform (w)
  (make-quickform 'question-form-view
		  :data-class-name 'question
		  :on-success
		  #'(lambda (qform q)
		      (declare (ignore qform))
		      (setq q (persist-object
			       *default-store*
			       q))
		      (setf (question-name q)
			    (question-prompt q)) ;xxx?
		      (group-editor-add-question w q))))

(defun group-action-create-new-subgroup (w)
  (labels ((open-new-group (g)
	     (let ((tabnav (find-workspace-widget w 'editor-tabnav)))
	       (do-pane tabnav g (make-survey-editor-view
				      tabnav g)
			(survey-object-edit-tokens g)))))
    (render-link (f* (do-dialog ""
		       (make-quickform 'survey-group-new-view
				       :data-class-name 'survey-group
				       :on-success
				       #'(lambda (qform g)
					   (declare (ignore qform))
					   (setq g (persist-object
						    *default-store*
						    g))
					   (setf (owner g) (current-user))
					   (open-new-group g)))))
		 "Create Subgroup")))

(defun group-action-undo (w)
  (unless (null (undo-history w))
    (render-link (f* (undo w)) "Undo")))

(defun group-action-redo (w)
  (unless (null (redo-history w))
    (render-link (f* (redo w)) "Redo")))

(defun group-action-save-changes (w)
  (when (or (undo-history w)
	    (redo-history w))
;;    (group-editor-question-widgets w)
    (render-link (f* (group-editor-commit-changes w))
		 "Save all changes")))


(defun group-action-revert-changes (w)
  (when (or (undo-history w)
	    (redo-history w))
    (render-link (f* (group-editor-discard-changes w))
		 "Revert all changes")))

(defun group-action-manage-editors (w)
  (render-link (f* (group-editor-edit-acl w))
	       "Manage Editors"))

;;
;; Proxy object for questions
;;


(defclass proxy-question ()
  ((id :initarg :id :initform (gensym))
   (name :accessor question-name :initarg :name :initform "New question")
   (prompt :accessor question-prompt :initarg :prompt
	   :initform "New question")
   (question-help :accessor question-help :initarg :help :initform "Help text")
   (data-help :accessor question-data-help :initarg :data-help :initform "")
   (data-type :accessor question-data-type :initarg :data-type
	      :initform :string)
   (data-subtype :accessor question-data-subtype :initarg :data-subtype
		 :initform nil)
   (data-constraint :accessor question-data-constraint :initarg :data-constraint
		    :initform nil)
   (view-type :accessor question-view-type :initarg :view-type :initform :auto)
   (choices :accessor question-choices :initarg :choices :initform nil)
   (parent :accessor parent :initarg :parent :initform nil)))

(defun make-proxy-question (question)
  (check-type question (or proxy-question question))
  (make-instance 'proxy-question
		 :name (question-name question)
		 :prompt (question-prompt question)
		 :help (question-help question)
		 :data-help (question-data-help question)
		 :data-type (question-data-type question)
		 :data-subtype (question-data-subtype question)
		 :data-constraint (question-data-constraint question)
		 :view-type (question-view-type question)
		 :choices (question-choices question)))

(defwidget rule-editor (widget)
  ((rule :accessor rule-editor-rule :initarg :rule)
   (ui-state :accessor rule-editor-ui-state
	     :initarg :ui-state :initform :view)))

(defmethod initialize-instance :after ((w rule-editor) &rest initargs)
  (declare (ignore initargs))
  (let* ((rule (rule-editor-rule w))
	 (target (group-rule-target rule)))
    (when (null target)
      (setf (rule-editor-ui-state w) :edit))))

#||
;; A rule is a DEFSTRUCT instance.  We can get away with treating it
;; as a CLOS object on SBCL, but on other implementations, we'll lose.
(defview rule-data-view (:type data)
  value
  target)

(defview rule-table-view (:type table)
  value
  target)

(defview rule-form-view (:type form :persistp nil :use-ajax-p nil)
  (value :present-as (dropdown :choices (compose #'question-choices
						 #'group-rule-question)))
  (target :present-as (dropdown :choices #'(lambda (r)
					     (declare (ignore r))
					     (get-instances-by-class 'survey-group))
				:label-key #'group-name)
	  :parse-as (mid)
	  :reader (compose #'mid #'group-rule-target)))

||#

(defun possible-answers (question)
  (case (question-data-type question)
    (:boolean
     '(:true :false))
    ((:choice :multichoice)
     (question-choices question))
    (t nil)))

(defmethod render-widget-body ((w rule-editor) &rest args)
  (declare (ignore args))
  (let* ((rule (rule-editor-rule w))
	 (target (group-rule-target rule))
	 (question (group-rule-question rule))
	 (group (parent question)))
#||
    ;; It might be worth turning rules into persistent objects, instead
    ;; of DEFSTRUCT instances.  This would let use the gridedit widget to
    ;; operate on rules.  It might be possible to customize things so that
    ;; we could use it with the way things are currently, but the machinery
    ;; really wants objects to have an mid slot.

    (flet ((query-fn (grid sort range &key countp)
	     (if countp
	       (length (group-rules group))
	       (mapcar #'make-rule-class (group-rules group)))))
      (render-widget (make-instance 'gridedit
				    :allow-sorting-p nil
				    :allow-pagination-p nil
				    :data-class 'rule-class
				    :item-data-view 'rule-data-view
				    :item-form-view 'rule-form-view
				    :on-query #'query-fn
				    :view 'rule-table-view)))

    (render-widget (make-instance 'dataform
				  :data-view 'rule-data-view
				  :form-view 'rule-form-view
				  :data rule
				  :on-success
				  (lambda (w)
				    (let ((r (dataform-data w)))
				      (when (and (group-rule-value r)
						 (group-rule-target r))
					(add-rule group question
						  (group-rule-value r)
						  (group-rule-target r)
						  nil :replace t))))))
||#
    (ecase (rule-editor-ui-state w)
      (:view
       (with-html
	 (:li
	  (str (format nil "When the user answers ~a, go to group \"~a\" "
		       (group-rule-value rule)
		       (aif (group-rule-target rule)
			  (group-name it)
			  it)))
	  (render-image-link 
	   (f* (setf (rule-editor-ui-state w) :edit))
	   "/pub/images/icons/file.png"
	   :title "Edit rule")
	  (render-image-link 
	   (f* (delete-rule group question (group-rule-value rule))
	       (update-rule-widgets (widget-parent w)))
	   "/pub/images/icons/action_delete.png"
	   :title "Delete rule"))))
      (:edit
       (flet ((process-form (&key value target &allow-other-keys)
		#+debug
		(format *trace-output* "~&for answer ~s go to group ~s~%" value
			(get-model 'survey-group (parse-integer target)))
		(when (or (string= value "")
			  (string= target ""))
		  (return-from process-form nil))
		(let* ((target-id (ignore-errors (parse-integer target))))
		  (if target-id
		    (setq target (get-model 'survey-group target-id))
		    (setq target "")))
		(when (eq :boolean (question-data-type question))
		  (setq value (cond ((equal value "TRUE") :true)
				    (t :false))))
		(when (and value target)
		  (setf (rule-editor-rule w)
			(add-rule group question value target :inline :replace t)))
		nil))
	 (with-html
	   (with-html-form (:post #'process-form)
	     (:li
	      "When the user answers: "
	      (let* ((other-args nil))
		(if (group-rule-value rule)
		  (setq other-args (list :selected-value (group-rule-value rule)))
		  (setq other-args (list :welcome-name "answer")))
		(apply #'render-dropdown "value" (possible-answers question)
		       :autosubmitp t other-args))
	      " select group "
	      (let* ((groups (get-legal-subgroups group))
		     (alist nil)
		     (other-args nil))
		(dolist (g groups)
		  (push (cons (abbreviated-string (group-name g))
			      (mid g)) alist))
		(if target
		  (setq other-args (list :selected-value (format nil "~d" (mid target))))
		  (setq other-args (list :welcome-name "subgroup")))
		(apply #'render-dropdown "target" alist :id "page"
		       :autosubmitp t other-args))
	      ;; (render-button "submit")
	      (render-link (lambda (&rest args)
			     (declare (ignore args))
			     (if (null (group-rule-target rule))
			       (progn
				 (delete-rule group question (group-rule-value rule))
				 (update-rule-widgets (widget-parent w)))
			       (progn
				 (setf (rule-editor-ui-state w) :view)
				 (mark-dirty (widget-parent w)))))
			   "[Done]")))))))))

(defwidget question-summary-widget (composite)
  ((question :accessor question-summary-question :initarg :question
	     :initform nil)
   (ui-state  :accessor question-summary-ui-state
	      :initarg :mode :initform :view)
   (selected-p :reader question-summary-selected-p
	       :initarg :selected-p :initform nil)
   (show-comments-p :accessor question-summary-show-comments-p
		    :initarg :show-comments-p :initform nil)))

(defmethod (setf question-summary-selected-p) (flag (qw question-summary-widget))
  (setf (slot-value qw 'selected-p) flag)
  (if flag
    (setf (dom-class qw) "selected")
    (setf (dom-class qw) nil)))

(defun make-question-summary-widget (question)
  (make-instance 'question-summary-widget
		 :question question
		 :dom-id (format nil "question_~d"
				 (mid question))))

(defun make-rule-widgets (question-summary)
  (let* ((question (question-summary-question question-summary))
	 (group (parent question)))
    (when group
      (loop for rule in (group-rules group)
	    when (eq (group-rule-question rule) question)
	    collect (make-instance 'rule-editor :rule rule)))))

(defmethod initialize-instance :after ((w question-summary-widget)
				       &rest initargs)
  (declare (ignore initargs))
  (setf (composite-widgets w) (make-rule-widgets w)))

(defun update-rule-widgets (question-summary)
  (setf (composite-widgets question-summary)
	(make-rule-widgets question-summary)))

(defmethod render-widget-body ((w question-summary-widget) &rest args)
  (declare (ignore args))
  (with-slots (question) w
    (let ((comments (get-comments question)))
      (with-html
	(:div :class "question-prompt"
	      (:span :class "question-prompt-text"
		     (str (question-prompt question))
		     (when (is-admin-p)
		       (str (format nil " (~a)" (mid question)))))
	      (:span :class "question-summary-actions"
		    (when comments
		      (render-image-link 
		       (f* (setf (question-summary-show-comments-p w)
				 (not (question-summary-show-comments-p w))))
		       "/pub/images/icons/comments.png"
		       :title "Show/Hide comments"))
		    (render-image-link (f* (do-edit-question-dialog w question))
				       "/pub/images/icons/file.png"
				       :title "Edit Question")
		    ;; If a question has no answers, let the user delete it.
		    ;; (What about when a deleted question's id is in the scratchpad?)
		    (when (null (get-answers question))
		      (render-image-link 
			(lambda/cc (&rest rest)
                          (declare (ignore rest))
                          (when (eq (do-confirmation "Are you sure you want to delete this question permanently?  This action cannot be undone." :type :yes/no)
                                    :yes)
                            (let ((parent (widget-parent w)))
                              (group-editor-delete-question parent question))))
			"/pub/images/icons/action_delete.png"
			:title "Delete question")))
	      (:div :style "clear: both;"))
	(:div :class "question-type"
	      (awhen (find-question-type question)
		(htm (:b "Question type: ")
		     (str (humanize-name it)))))
	(:div :class "question-help"
	      (:b (str #!"Help")) (str (question-help question))))
      (with-html
	(:div :class "question-input"
	      (:p "Subgroups:")
	      (:ul
	       (mapc #'render-widget (composite-widgets w))
	       (when (member (question-data-type question)
			     '(:boolean :choice :multichoice))
		 (htm
		  (:li
		   (render-image-link 
		    (f* (add-rule (parent question) question nil nil :inline)
			(update-rule-widgets w))
		    "/pub/images/icons/action_add.png"
		    :title "Add new subgroup")))))
	      (awhen (activates-inline-group-p question)
		(htm 
		 (:p (str #!"Edit sub-groups: ")
		     (loop for group in it do
			  (when group
			    (render-link (lambda (&rest args)
					   (declare (ignore args))
					   (let ((tabnav (find-workspace-widget w 
										'editor-tabnav)))
					     (do-pane tabnav group 
						      (make-survey-editor-view
						       tabnav group)
						      (survey-object-edit-tokens group))))
					 (group-name group))
			    (with-html "&nbsp; &nbsp;")))))))
	(when (question-summary-show-comments-p w)
	  (htm (:p (render-comments comments))))
	(send-script "groupEditInitialize();")))))

(defun items-from-id-list (id-list class-name)
  (mapcar #'(lambda (id) (get-model class-name id)) id-list))

(defun re-order-list (old-list new-order key)
  (mapcar #'(lambda (id)
	      (find id old-list :key key))
	  new-order))

(defun parse-sortable-output (string)
  "Extract lisp integers from the Sortable serializer output"
  (mapcar #'(lambda (string)
	      (read-from-string
	       (subseq string 1)))
	  (cl-ppcre:all-matches-as-strings "=(\\d+)[^&]" string)))

(defparameter *lookup-parameters*
  '((:text-field :string :text-field)
    (:paragraph :string :paragraph)
    (:number    :number :text-field)
    (:radio-number :number :radio)
    (:measurement :number :measurement)
    (:date      :date   :text-field)
    (:checkbox  :boolean :checkbox)
    (:yes/no    :boolean :radio)
    (:choices   :choice :dropdown)
    (:radio-choices :choice :radio)
    (:multiple-choices :multichoice :multiple-dropdown))
  "(ui-text ui-term dtype vtype)")

(defun find-question-type (question)
  (ignore-errors ;; NOTE: quick and dirty implementation, soft fail in UI
    (first (or (find (list (question-data-type question)
			   (question-view-type question))
		     *lookup-parameters*
		     :key #'cdr
		     :test #'equal)
	       (and (eq (question-view-type question) :auto)
		    (find (list (question-data-type question)
				(default-view-for-type 
				    (question-data-type question)))
			  *lookup-parameters*
			  :key #'cdr
			  :test #'equal))))))


(defun group-editor-reorder-questions (group-editor sortable-string)
  (let ((id-list (parse-sortable-output sortable-string))
	(session-group (session-object (group-editor-group group-editor))))
    (save-undo-state group-editor)
    ;; update question list
    (setf (group-questions session-group) (items-from-id-list id-list 'question))
    ;; update list of question widgets
    (update-question-summary-widgets group-editor)))

(defun group-editor-update-selection (w id)
  ;;(format *trace-output* "~&selection now: ~s~%" id)
  (dolist (qw (group-editor-question-widgets w))
    (if (string= id (dom-id qw))
      (setf (question-summary-selected-p qw) t)
      (setf (question-summary-selected-p qw) nil))))

;; ======================================
;;  Question Objects and Editing View
;; ======================================


(defmethod survey-object-edit-url ((question question))
  (format nil "/dashboard/create/question/~A" (mid question)))

(defmethod survey-object-edit-tokens ((question question))
  (list "question" (format nil "~A" (mid question))))

(defmethod make-survey-editor-view (nav (question question))
  (if (not (editable-by-user-p question))
    "You don't have permission to edit this question."
    (let ((qw (make-question-editor-widget question)))
      (setf (widget-parent qw) nav)
      qw)))

;; sidebar actions for question editor

(defun question-view-translation (widget)
  (declare (ignore widget))
  (do-information "question-view-translation unimplemented"))

(defun question-view-comments (widget)
  (declare (ignore widget))
  (do-information "question-view-comments unimplemented"))

(defun question-view-rules (widget)
  (declare (ignore widget))
  (do-information "question-view-rules unimplemented"))

(defun question-delete-question (widget)
  (declare (ignore widget))
  (do-information "question-delete-question unimplemented"))

(defwidget question-editor-widget (widget undo-redo-mixin)
  ((question :accessor question-editor-question :initarg :question
	    :initform nil)
   (ui-state :accessor question-editor-ui-state :initform :view)
   (values :accessor question-editor-values :initarg :values :initform "")
   (show-values-p :initarg :show-values-p :initform nil)
   (show-comments-p :initarg :show-comments-p :initform nil)
   (error-string :initform nil :accessor question-editor-error-string)))

(defmethod initialize-instance :after ((w question-editor-widget) &rest args)
  (declare (ignore args))
  (with-slots (question) w
    (setf (session-object question) (make-proxy-question question))))

(defun make-question-editor-widget (question)
  (make-instance 'question-editor-widget
		 :question question))

(defmethod dependencies append ((widget question-editor-widget))
  (list (make-local-dependency :stylesheet "question-editor-widget")))

(define-condition missing-choices-error (error)
  ((missing-choices :initarg :missing-choices
                    :accessor missing-choices))
  (:report (lambda (c s)
             (format s "Mising choices: ~s" (missing-choices c)))))

(defun question-answer-values (question)
  (let ((vals nil))
    (dolist (answer (get-instances-by-value 'answer 'question question))
      (let ((val (value answer)))
        (if (stringp val)
          (pushnew val vals :test #'string=)
          (dolist (s val) (pushnew s vals :test #'string=)))))
    vals))

(defun validate-question-editor-changes (question session-question)
  (when (member (question-data-type session-question) '(:choice :multichoice))
    (let ((session-choices (mapcar #'cdr (question-choices session-question)))
          (choices (question-choices question)))
      (unless (and (member (question-data-type question) '(:choice :multichoice))
                   (eql (length session-choices) (length choices))
                   (null (set-difference (mapcar #'cdr choices) session-choices
                                         :test #'string=)))
        (let* ((values (question-answer-values question))
               (missing (set-difference values session-choices :test #'string=)))
          (when missing
            (error 'missing-choices-error :missing-choices missing)))))))

(defun question-editor-commit-changes (question-editor)
  (with-slots (question) question-editor
    (let ((session-question (session-object question)))
      (validate-question-editor-changes question session-question)
      (clear-session-object question)
      (clear-undo-state question-editor)
      (ensure-transaction ()
	(setf (question-name question) (question-name session-question))
	(setf (question-prompt question) (question-prompt session-question))
	(setf (question-help question) (question-help session-question))
	(setf (question-data-help question)
	      (question-data-help session-question))
	(setf (question-data-type question)
	      (question-data-type session-question))
	(setf (question-data-subtype question)
	      (question-data-subtype session-question))
	(setf (question-view-type question)
	      (question-view-type session-question))
	(setf (question-choices question)
	      (question-choices session-question)))
      (when (typep question 'question)
        (record-event :question-changed question)))))

(defun question-editor-discard-changes (question-editor)
  (with-slots (question) question-editor
    (clear-session-object question)
    (clear-undo-state question-editor)))

(defmethod save-undo-state ((question-editor question-editor-widget))
  (with-slots (question undo-history) question-editor
    (push (make-proxy-question (session-object question))
	  undo-history)))

(defmethod undo ((question-editor question-editor-widget))
  (with-slots (undo-history redo-history question) question-editor
    (unless (null undo-history)
      (push (session-object question) redo-history)
      (setf (session-object question) (pop undo-history)))))

(defmethod redo ((question-editor question-editor-widget))
  (with-slots (redo-history undo-history question) question-editor
    (unless (null redo-history)
      (push (session-object question) undo-history)
      (setf (session-object question) (pop redo-history)))))

(defmethod render-widget-body ((w question-editor-widget) &rest args)
  (declare (ignore args))
  (let ((session-question (session-object (question-editor-question w))))
    (with-slots (question ui-state) w
      ;; preview
      (with-html 
	(:span :class "question-preview-title"
;;	       :style "font-size: small; margin-bottom: 5px;"
	       (str #!"Question preview: "))
	(:div :class "question-preview"
	      (:p :class "question-prompt"
		  (str (question-prompt session-question)))
	      (:p :class "question-help"
		  (str (question-help session-question)))
	      (:div :class "question-input" 
		    (present-question-hack session-question))))
      (flet ((process-form (&key question-prompt question-help
				 cancel &allow-other-keys)
	       (when cancel
		 (setf ui-state :view)
		 (return-from process-form))
	       (when (or question-prompt question-help)
		 (let ((pq (make-proxy-question session-question)))
		   (when question-prompt
		     (setf (question-prompt pq) question-prompt))
		   (when question-help
		     (setf (question-help pq) question-help))
		   (save-undo-state w)
		   (setf (session-object question) pq))
		 (setf ui-state :view))))
	(ecase ui-state
	  (:view 
	   (with-html
;;	     (:label "Question prompt: "
;;		     (:blockquote
;;		      (str (question-prompt session-question))))
;;	     (:label "Question help: "
;;		     (:blockquote (str (or (question-help session-question)
;;					   "[empty help text]"))))
	     (render-link #'(lambda (&rest args)
			      (declare (ignore args))
			      (setf ui-state :edit))
			  "[edit]")))
	  (:edit
	   (with-html-form (:post #'process-form)
	     (:hr)
	     (:div :class "question-header-form-fields"
		   (:label "Question prompt: "
			   (render-textarea "question-prompt"
					    (question-prompt session-question) 3 30))
		   (:label "Question help: "
			   (render-textarea "question-help"
					    (question-help session-question) 3 30)))
	     (:div :class "question-header-form-buttons"
		   (render-translated-button "Submit")
		   (render-translated-button "Cancel"))))))

      (with-html
	(:hr))

      (render-answer-type-selector w)
      (maybe-render-unit-selector w)
      (maybe-render-values-widget w)
      (let ((error-string (question-editor-error-string w)))
        (when error-string
          (with-html
            (:p :class "validation-error" (str error-string)))))
      (with-html
	(:hr))
      (unless (null (undo-history w))
	(render-link (lambda (&rest args)
		       (declare (ignore args))
		       (undo w))
		     "[undo] "))
      (unless (null (redo-history w))
	(render-link (lambda (&rest args)
		       (declare (ignore args))
		       (redo w))
		     "[redo] "))
      (if (or (undo-history w) (redo-history w))
	(progn
	  (render-link (lambda (&rest args)
			 (declare (ignore args))
                         (handler-case
                             (progn (setf (question-editor-error-string w) nil)
                                    (question-editor-commit-changes w)
                                    (answer w))
                           (missing-choices-error (c)
                             (setf (question-editor-error-string w)
                                   (format nil
                                           "Missing choices in existing answers: ~
                                            ~s~{,~s~}"
                                           (car (missing-choices c))
                                           (cdr (missing-choices c)))))))
		       "[Close, saving changes] ")
	  (render-link (lambda (&rest args)
			 (declare (ignore args))
			 (question-editor-discard-changes w)
			 (answer w))
		       "[Close, discarding changes]"))
	(render-link (lambda (&rest args)
		       (declare (ignore args))
		       (answer w))
		     "[Close]"))
      )))

(defun present-question-hack (question &optional name)
  (case (question-data-type question)
    (:number
     (case (question-view-type question)
       (:radio
	(let ((pair (question-data-constraint question)))
	  (when (null pair)
	    (setf pair (cons 1 5)
		  (question-data-constraint question) pair))
	  (present-as 'number-radio-presentation (car pair) :query-name name
		      :low-value (car pair) :high-value (cdr pair))))
       (:number
	(present-as 'number-presentation 20.0 :query-name name))
       (:measurement
	(let* ((unit (or (canonical-unit-for-measurement 
			  (question-data-subtype question))
			 "kg")))
	  ;; The use of "kg" as the unit is totally arbitrary.
	  ;; We just need *some* default initial value.
	  (present-as 'measurement-presentation 40 :query-name name
		      :display-unit unit
		      :canonical-unit unit)))))
    (:string 
     (case (question-view-type question)
       ((:auto :text-field)
	(present-as 'string-presentation "This is some text" :query-name name))
       (:paragraph (present-as 'text-area-presentation "This is some text"
			       :query-name name))))
    (:date (present-as 'date-presentation (get-universal-time)
		       :query-name name))
    (:boolean
     (case (question-view-type question)
       ((:auto :radio)
	(present-as 'radio-boolean-presentation t :query-name name))
       (:checkbox
	(present-as 'checkbox-boolean-presentation t :query-name name))))
    (:choice
     (case (question-view-type question)
       (:radio (present-as 'member-radio-presentation nil
			   :query-name name
			   :choices (or (question-choices
					 (session-object question))
					'(("Choice1" . "choice1")
					  ("Choice2" . "choice2")
					  ("Choice3" . "choice3")))))
       ((:auto :dropdown)
	(present-as 'member-select-presentation nil
		    :query-name name
		    :test-function #'equalp
		    :choices (or (question-choices
				  (session-object question))
				 '(("Choice1" . "choice1")
				   ("Choice2" . "choice2")
				   ("Choice3" . "choice3")))))))
    (:multichoice (present-as 'multiple-members-select-presentation nil
			      :query-name name
			      :choices  (or (question-choices
					     (session-object question))
					    '(("Choice1" . "choice1")
					      ("Choice2" . "choice2")
					      ("Choice3" . "choice3")
					      ("Choice4" . "choice4")))))))
		   
(defun lines-to-alist (str)
  (with-input-from-string (s str)
    (loop for line = (read-line s nil)
	  while (and line (plusp (length line)))
	  do (setq line (string-trim '(#\space #\tab #\newline) line))
	  collect (cons line line) into alist
	  finally (return alist))))

(defun alist-to-lines (alist)
  (format nil "~{~a~%~^~}" 
	  (mapcar (lambda (choice)
		    (if (consp choice) 
			(car choice)
			choice))
		  alist)))

(defun maybe-render-values-widget (question-editor-widget)
  (with-slots (question) question-editor-widget
    (let ((session-question (session-object question)))
      (cond ((and (eq (question-data-type session-question) :number)
		  (eq (question-view-type session-question) :radio))
	     (flet ((process-form (&key low high &allow-other-keys)
		      (let* ((x (ignore-errors (parse-integer low)))
			     (y (ignore-errors (parse-integer high))))
			(when (and x y (< x y))
			  (let ((pq (make-proxy-question session-question)))
			    (setf (question-data-constraint pq) (cons x y))
			    (save-undo-state question-editor-widget)
			    (setf (session-object question) pq)
			    (mark-dirty question-editor-widget))))))
	       (with-html
		 (with-html-form (:post #'process-form)
		   (:p
		    (:label (str "low value")
			    (render-text-input "low" (format nil "~a" (car (question-data-constraint session-question))))))
		   (:p
		    (:label (str "high value")
			    (render-text-input "high" (format nil "~a" (cdr (question-data-constraint session-question))))))
		   (:p
		    (render-translated-button "submit"))))))
	    ((member (question-data-type session-question)
		     '(:choice :multichoice))
	     (flet ((process-form (&key text &allow-other-keys)
		      (when text
			(let ((pq (make-proxy-question session-question)))
			  (setf (question-choices pq) (lines-to-alist text))
			  (save-undo-state question-editor-widget)
			  (setf (session-object question) pq)
			  (mark-dirty question-editor-widget)))))
	       (with-html-form (:post #'process-form)
		 (render-textarea "text" (alist-to-lines (question-choices session-question)) 8 80)
		 (render-translated-button "submit"))))))))

;; There are two kinds of "type" associated with a question's answer:
;; the type of the data (e.g., boolean, string, number, etc.) and the
;; type of the presentation (e.g., text-field, text area, checkbox, etc.)
;; We sort of conflate the two here.

(defun maybe-render-unit-selector (question-widget)
  (let* ((question (session-object
		    (question-editor-question question-widget))))
    (when (eq (question-view-type question) :measurement)
      (flet ((process-form (&key unit &allow-other-keys)
	       (update-measurement-unit question-widget unit)))
	(with-html
	  (with-html-form (:post #'process-form)
	    (:label (str "Measurement unit")
		    (render-dropdown "unit" (mapcar #'(lambda (x)
							(humanize-name (car x)))
						    *canonical-units*)
				     :autosubmitp t
				     :selected-value (question-data-subtype question)))))))))
  

(defun valid-answer-types (question)
  (let* ((has-answer (get-instance-by-value 'answer 'question question))
	 (dtype (question-data-type question)))
    (when (eq dtype :auto)
      (setq dtype (default-view-for-type dtype)))
    (if has-answer
      (remove-if-not #'(lambda (x) (eq (cadr x) dtype)) *lookup-parameters*)
      *lookup-parameters*)))

(defun render-answer-type-selector (question-widget)
  (let* ((real-question (question-editor-question question-widget))
         (question (session-object real-question))
	 (parameters (valid-answer-types question)))
    (with-html
      (:label (str (if (has-answer-p real-question)
                       #!"Question answer type (existing answers will disallow some changes): "
                       #!"Question answer type: "))
        (:table :id "type-selector"
	  (loop for pair in (group parameters 2) do
		(htm (:tr
		      (destructuring-bind (tag dtype vtype) (first pair)
			(htm (:td (render-link 
				   (lambda (&rest args)
				     (declare (ignore args))
				     (update-answer-type question-widget 
							 dtype vtype))
				   (humanize-name tag)))))
		      (when (second pair)
			(destructuring-bind (tag dtype vtype) (second pair)
			  (htm (:td (render-link 
				     (lambda (&rest args)
				       (declare (ignore args))
				       (update-answer-type question-widget 
							   dtype vtype))
				     (humanize-name tag))))))))))))))


(defun render-fancy-answer-type-selector (question-widget)
  "Render a grid of answer presentations that are compatible with the type
   of QUESTION's answer.  If QUESTION has no answers, show everything."
  ;; The docstring lies.
  (macrolet ((when-data-type-member (type &body body)
               `(when (member (question-data-type (question-editor-question question-widget))
                              (list ,@(if (listp type)
                                          type
                                          (list type))
                                     nil))
                  ,@body))
             (type-row (lisp-type &rest presentation-types)
               `(with-html
                  (:tr (:td ,lisp-type)
                       (:td (:table
                             ,@(loop
                                 for (name . body) in presentation-types
                                 collect `(:tr (:td (:p (:input :type "radio" :name "answer-data-type" :value ,name)
                                                        (:label :for ,name (str ,name)))
                                                    (with-html ,@body))))))))))
;;     (let ((code (make-action (lambda (&key dtype vtype &allow-other-keys)
;; 			       (update-answer-type question-widget
;; 						   dtype vtype)))))
;;       (with-html
;; 	(:input :id "answer-type-action-code" :type "hidden" :value code)))
    (with-html
      (:table :id "answer-types"
              (when-data-type-member :boolean
                (type-row "Boolean"
                          ("Checkbox" (present-as 'checkbox-boolean-presentation t))
                          ("Radio" (present-as 'radio-boolean-presentation nil))))
              (when-data-type-member :string
                (type-row "Text"
                  ("One line of text" (present-as 'short-string-presentation "hello."))
                  ("One or more paragraphs of text" (present-as
                  'text-area-presentation "Lorem ipsum dolor sit amet,
                  consectetuer adipiscing elit. Sed quis
                  eros. Vestibulum consectetuer rutrum libero. Morbi
                  elit diam, convallis vel, auctor sit amet, ornare
                  in, urna. In tempor. Mauris magna sem, varius et,
                  elementum a, iaculis sit amet, sem. Ut nec neque
                  quis ante tincidunt euismod.

                  Morbi mattis semper magna. Curabitur eu ante lacinia
                  ligula venenatis fringilla. In sit amet
                  sem. Praesent cursus rhoncus tellus. Vestibulum
                  lobortis tincidunt est. Vivamus sit amet ipsum at
                  risus eleifend hendrerit. Morbi a risus. Ut tempus,
                  lorem eu sagittis hendrerit, nulla leo tempus lorem,
                  et ultrices urna diam vel justo."))))
              (when-data-type-member (:number :range)
                (type-row "Numbers"
                          ("Any number" (present-as 'integer-presentation 42 :precision 1))
                          ("Any integer" (present-as 'number-presentation 17.3))
                          ("Number bounded by maximum and minmum values."
                           (present-as 'range-limited-number-presentation 4.3 :min-value 0 :max-value 10))
                          ("Integer bounded by maximum and minmum values."
                           (present-as 'range-limited-integer-presentation 4 :min-value 0 :max-value 10))))
              (when-data-type-member (:choice :multichoice)
                (type-row "Multiple choice."
                          ("Exactly one answer, radio buttons."
                           (present-as 'member-radio-presentation "b" :choices '("a" "b" "c")))
                          ("Exactly one answer, from a list (aka dropdown box)."
                           (present-as 'member-select-presentation "b" :choices '("a" "b" "c")))
                          ("Multiple answers, from a list."
                           (present-as 'multiple-members-select-presentation "b"
                                       :choices '("a" "b" "c")))))
              (when-data-type-member :date
                (type-row "Date and/or time."
                          ("A datetime" (present-as 'datetime-presentation (get-universal-time)))
                          ("A date" (present-as 'date-presentation (get-universal-time)))
                          ("A time" (present-as 'time-presentation (get-universal-time)))))))))

(defun update-answer-type (question-editor dtype vtype)
  (with-slots (question) question-editor
    (let ((dtype (intern (string-upcase dtype) "KEYWORD"))
	  (vtype (intern (string-upcase vtype) "KEYWORD"))
	  (session-question (session-object question)))
      (save-undo-state question-editor)
      (setf (question-data-type session-question) dtype)
      (setf (question-view-type session-question) vtype))))

(defun update-measurement-unit (question-editor unit)
  (with-slots (question) question-editor
    (let ((unit (intern (string-upcase unit) "KEYWORD"))
	  (session-question (session-object question)))
      (save-undo-state question-editor)
      (setf (question-data-subtype session-question) unit))))
