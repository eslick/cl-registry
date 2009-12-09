(in-package :registry)

(registry-proclamations)

;; =========================================================
;;  Workspace base class
;; =========================================================

(defwidget workspace ()
  ((widget-table :accessor widget-table :initform (make-hash-table :test 'equalp))
   (flows :accessor widget-flow-table :initform (make-hash-table :test 'equalp))
   (main-pane :accessor main-pane-name :initform nil)
   (sidebar :accessor sidebar-names :initform nil)))

(defmethod dependencies append ((wksp workspace))
  (list (make-local-dependency :stylesheet "table")
	(make-local-dependency :stylesheet "datagrid")))

(defmethod set-main-pane ((workspace workspace) name widget)
  (assert (not (eq name 'main-pane)))
  (awhen (find-workspace-widget workspace (main-pane-name workspace))
    (setf (widget-parent (find-workspace-widget workspace it)) nil))
  (setf (main-pane-name workspace) name)
  (setf (widget-parent widget) workspace)
  (setf (gethash name (widget-table workspace)) widget)
  (setf (gethash name (widget-flow-table workspace)) (mklist widget)))

(defmethod set-sidebar-widgets ((workspace workspace) &rest widgets)
  (dolist (name (sidebar-names workspace))
    (setf (widget-parent (find-workspace-widget workspace name)) nil))
  (setf (sidebar-names workspace) nil)
  (apply 'append-sidebar-widgets workspace widgets))

(defmethod append-sidebar-widgets ((workspace workspace) &rest widgets)
  (let* ((tuples (group widgets 2)))
    (dolist (pair tuples)
      (destructuring-bind (name widget) pair
	(setf (gethash name (widget-table workspace)) widget)
	(setf (gethash name (widget-flow-table workspace)) (mklist widget))
	(setf (widget-parent widget) workspace)))
    (setf (sidebar-names workspace) 
	  (remove-duplicates
	   (append (sidebar-names workspace)
		   (cars tuples))))))


;; =========================================================
;;  Accessors 
;; =========================================================

(defun get-workspace-widget (workspace name)
  (gethash name (widget-table workspace)))
(defun set-workspace-widget (widget workspace name)
  (setf (gethash name (widget-table workspace)) widget))
(defsetf get-workspace-widget set-workspace-widget)

(defun get-workspace-widgets (workspace names)
  (mapcar (curry #'get-workspace-widget workspace) names))

(defun get-main-widget (workspace)
  (get-workspace-widget workspace (main-pane-name workspace)))

(defun get-sidebar-widgets (workspace)
  (get-workspace-widgets workspace (sidebar-names workspace)))

(defun find-workspace-name (workspace widget)
  (awhen (find widget (hash-items (widget-table workspace)) :key #'cdr)
    (car it)))

(defun get-flow-stack (workspace name)
  (gethash name (widget-flow-table workspace)))

;; =========================================================
;;  Workspace client methods
;; =========================================================

(defun find-parent-workspace (self &optional (type 'workspace))
  "Find the parent workspace object starting at this widget"
  (if (subtypep (type-of self) type) self
      (aif (widget-parent self)
	   (find-parent-workspace it type)
	   (error "No workspace in parent path"))))

(defun find-workspace-widget (self name)
  "Find a widget by name in the workspace the widget resides"
  (when name
    (let ((workspace (find-parent-workspace self)))
      (when (eq name 'main-pane)
	(setf name (main-pane-name workspace)))
      (get-workspace-widget workspace name))))

;; =========================================================
;;  Flow support
;; =========================================================

(defmethod map-subwidgets progn (function (workspace workspace))
  "For tree walking through the workspace"
  (maphash (f (key value) 
	     (declare (ignore key))
	     (funcall function (first value)))
	   (widget-flow-table workspace)))

(defmethod make-widget-place-writer ((workspace workspace) widget)
  "Returns a place writer for purposes of flows"
  (let ((name (find-workspace-name workspace widget)))
    (lambda (&optional callee)
      (setf (widget-parent callee) workspace)
      (setf (gethash name (widget-table workspace)) callee)
      (if (eq callee (first (get-flow-stack workspace name)))
	  (pop (gethash name (widget-flow-table workspace)))
	  (push (gethash name (widget-flow-table workspace)) callee))
      (mark-dirty workspace))))

;; NOTE: We can track stack-like flows by pushing called widgets
;; onto the name in the flow table.  This allows us to implement
;; aborts.

(defun has-active-flow (workspace name)
  "Determines if there is an active flow at name"
  (> (length (get-flow-stack workspace name)) 1))

(defun abort-active-flow (workspace name)
  "Takes the currently active widget in the main pane"
  (let* ((stack (get-flow-stack workspace name))
	 (unwind-widgets (subseq stack 0 (1- (length stack)))))
    (dolist (widget unwind-widgets)
      (setf (widget-parent widget) nil))
    (set-main-pane workspace name (last1 stack))))

;; =========================================================
;;  Rendering protocol
;; =========================================================

(defmethod render-widget-body ((workspace workspace) &rest args)
  (declare (ignore args))
  (with-html 
    (:div :class "workspace-main-pane"
	  (render-widget (get-main-widget workspace)))
    (:div :class "workspace-sidebar"
	  (:div :class "top"
		(mapcar #'render-widget (get-sidebar-widgets workspace)))
	  (:div :class "bottom"))
    (:div :class "float-end")))




  



