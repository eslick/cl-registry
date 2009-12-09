(in-package :registry)

(registry-proclamations)

;; =========================================================
;;  Sidebar widgets
;; =========================================================

;; ========================================================
;;  Survey editing command widget
;; ========================================================

(defparameter *survey-editor-actions*
  '((survey-editor-widget  ;; for widget -> 
;;     survey-action-modify-header
     survey-action-add-new-page
     survey-action-add-existing-page
     survey-action-undo
     survey-action-redo
     survey-action-save-changes
     survey-action-revert-changes
     survey-action-diary-conversion
     survey-action-manage-editors
     )
    (group-editor-widget ;; for widget -> actions
     group-action-add-new-question
     group-action-create-new-subgroup
     group-action-preview-page
     group-action-undo
     group-action-redo
     group-action-save-changes
     group-action-revert-changes
     group-action-view-comments
     group-action-modify-header
     group-action-manage-editors)
    (question-editor-widget)))

(defun make-editor-action-panel (&optional actions)
  (make-instance 'editor-action-panel :actions actions))

(defwidget editor-action-panel ()
  ((actions :accessor actions :initarg :dictionary)))

(defmethod render-widget-body ((panel editor-action-panel) &rest args)
  (declare (ignore args))
  (let* ((editor (current-editor-view panel))
	 (editor-type (type-of editor)))
    (with-html
      (:h2 "Actions")
      (:ul
       (dolist (action (cdr (assoc editor-type *survey-editor-actions*)))
	 (htm (:div :class "action-link" 
		    (funcall action editor))))))))

(defun current-editor-view (panel)
  (let* ((tabnav (find-workspace-widget panel 'editor-tabnav)))
    (dynamic-pane-widget (current-dynamic-pane tabnav))))
    

;; =========================================================
;;  Search actions
;; =========================================================


(defun make-search-actions (actions &rest args)
  (apply #'make-instance 'sidebar-search-actions :actions actions args))

(defwidget sidebar-search-actions () 
  ((search-handler :accessor sidebar-search :initarg :handler
		   :initform 'default-search-handler)
   (maxlength :accessor sidebar-search-maxlength :initarg :maxlength :initform 60)
   (actions :accessor sidebar-actions :initarg :actions)))

(defun default-search-handler (widget query-string)
  (declare (ignore widget))
  (make-search-results
   (fulltext-search query-string)
   (lambda (result-widget item)
     (format t "~A" item)
     (answer result-widget))))

(defmethod render-widget-body ((widget sidebar-search-actions) &rest args)
   (declare (ignore args))
   (with-html-form (:post (lambda (&rest args &key query-string &allow-other-keys)
			    (format t "~A~%" args)
			    (do-widget (find-workspace-widget widget 'editor-tabnav)
			      (funcall (sidebar-search widget) widget query-string))))
     (:label :for "query-string" "Search")
     (render-text-input "query-string" "Search" :maxlength (sidebar-search-maxlength widget)
			:class "sidebar-search-input"))
   (with-html
     (:ul
      (dolist (action-record (sidebar-actions widget))
	(destructuring-bind (link-name action-fn) action-record
	  (htm (:li (render-link (lambda (&rest args)
				   (declare (ignore args))
				   (safe-funcall action-fn widget))
				 link-name :class "sidebar-action-link"))))))))

			  
;; 			      (lambda (result-widget result)
;; 				(abort-flow (find-workspace-main-widget widget))
;; 				(typecase result
;; 				  (survey (goto-survey 
					    

;; can this be a datalist or dataseq??

(defun make-search-results (results handler &optional group)
  (make-instance 'search-results-widget 
		 :results results
		 :handler handler
		 :group group))

(defwidget search-results-widget ()
  ((results :accessor search-results :initarg :results)
   (group :accessor group-results :initarg :group :initform nil
	  :documentation "Group objects by class")
   (action-handler :accessor action-handler :initarg :handler :initform nil
		   :documentation "Accepts search results widget and a result object.
                                   When the user clicks on a result, what happens?")))

(defun make-search-result-action (widget result)
  (make-action (lambda (&key cancel &allow-other-keys)
		 (if cancel
		     (answer widget)
		     (safe-funcall (action-handler widget) widget result)))))

(defmethod render-widget-body ((widget search-results-widget) &rest args)
  (declare (ignore args))
  (with-html
    (:ul
     (dolist (result (search-results widget))
       (htm (:li (render-link (lambda (&rest args)
				(declare (ignore args))
				(funcall (action-handler widget) widget result))
			      (question-name result))))))))

(defun make-scratchpad (&rest args)
  (apply #'make-instance 'sidebar-scratchpad args))

(defwidget sidebar-scratchpad (widget)
  ((id-list :accessor scratchpad-id-list :initarg :id-list :initform nil)))

(defun abbreviated-string (s &optional (len 30))
  (if (<= (length s) len)
    s
    (concatenate 'string (subseq s 0 len) "...")))

(defmethod render-widget-body ((w sidebar-scratchpad) &rest args)
  (declare (ignore args))
  (let ((code (make-action #'(lambda (&key item &allow-other-keys)
			       (scratchpad-update-handler w item)))))
    (with-html
      (:h2 "Scratchpad")
      (:ul :id "scratchpad-list"
	   :class (unless (scratchpad-id-list w) "empty")
	(dolist (item (scratchpad-id-list w))
	  (let ((an-item item)          ;binding to close over below
                (item-id (format nil "scratchpad_~a" item))
		(obj (or (get-model 'question item)
                         (get-model 'survey-group item))))
	    (htm
	     (:li :id item-id :class (if (typep obj 'question)
				       "scratchpad-question"
				       "scratchpad-group")
		  (typecase obj
		    (question
		     (htm (:b (str (format nil #!"Question (~d)" item)))
			  (:br)
			  (str (abbreviated-string
                                (slot-value-translation obj 'prompt)))))
		    (survey-group
		     (htm (:b (str (format nil #!"Page (~d)" item)))
			  (:br)
			  (str (abbreviated-string
                                (slot-value-translation obj 'name))))))
		  #+nil
		  (str (cl-who:escape-string
			(format nil "~s" obj)))
                  (render-link #'(lambda (&rest args)
                                   (declare (ignore args))
                                   (setf (scratchpad-id-list w)
                                         (remove an-item (scratchpad-id-list w)))
                                   (mark-dirty w))
                               " [remove]")))
	    (send-script (format nil "scratchpadEnableDrag('~a')" item-id)))))
      (:a :id (format nil "~a-handler" (dom-id w))
	  :handler code))))

;;  (send-script "scratchpadInitialize()")
;;  #+broken
;;  (push (weblocks::json-function "scratchpadInitialize()")
;;	*on-ajax-complete-scripts*))

;; ITEM will be of the form "something_1234"
(defun scratchpad-update-handler (w item)
  (let* ((n (position "_" item :from-end t :test #'string=))
	 (id (parse-integer item :start (1+ n)))
	 (obj (or (get-model 'survey-group id) (get-model 'question id))))
    (etypecase obj
      (survey-group ())
      (question (when (or (not (slot-boundp obj 'parent))
			  (null (slot-value obj 'parent)))
		  (let ((gw (find-group-editor-widget w)))
		    (if gw
		      (progn
			(setf (parent obj) (group-editor-group gw))
			(format *trace-output* "~&set parent of ~s to ~s~%"
				obj (group-editor-group gw)))
		      (error "can't find parent for ~s" obj))))))
    (pushnew id (scratchpad-id-list w))))

;; ==========================================
;;  Navigation
;; ==========================================

(defwidget survey-navigation ()
  ((survey :accessor survey :initarg :survey)))
