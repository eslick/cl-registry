(in-package :registry)

;;
;; View elements for bundles
;;
;; We can get away with less widgets
;;

;; Creation protocol:
;; 1) When a page bundle is to be rendered, we first walk it 
;;    and generate a set of widgets to render the content.
;; 2) Bundles do not necessarily translation 1:1 to widgets
;; 3) Configuration of widgets is set based on current rendering context
;;    e.g. the depth of the widget in a possible layout
;; 4) Presentation types are created for embedded questions
;; 5) Any layout/presentation constraints (i.e. CSS) are applied to 
;;    set the configuration of the widgets and questions

;; Rendering protocol:
;; 
;; The rendering protocol is now fairly simple, we simply accommodate the 
;; parameterized widget's render-body method to render its' context and
;; then render the children.

;;
;; SIMPLE CONTEXT 
;;
;; Track my path in the tree
;; Provides my parent, and my current depth...
;;

(defclass rendering-context ()
  ((path :accessor context-path :initarg :path :initform nil)))

(defun make-rendering-context (&rest args)
  (apply #'make-instance 'rendering-context args))

(defmethod update-context (widget context system)
  (make-rendering-context
   :path (append widget (context-path context))))

(defun context-depth (context) (length (context-path context)))
(defun context-parent (context) (first (context-path context)))


;;
;; BASE WIDGET CLASS
;;
;; We should not need to touch the DB to render a page, everything should
;; be cached.
;; Children are automatically rendered so default render method is nil.

(defwidget bundle-widget ()
  ((bundle :accessor bundle :initarg :bundle :initform nil
	   :documentation "A reference to the defining bundle for debugging")
   (layout-info :accessor layout-info :initarg :layout-info :initform nil
		:documentation "A reference to any CSS-style constraints")
   (value-cache :accessor value-cache :initarg :value-cache :initform nil
		:documentation "To avoid accesses to the DB, an alist of 
                                text to render based on current translation"))
  (:documentation "Abstract base-class"))


;; Called by a widget which wishes to display page content or forms
;; Could also be done via a cascading stylesheet model w/ default hints,
;; removing need for a presentation system override.
(defun make-bundle-widget-tree (top-bundle stylesheet system)
  "Main entry point to buildling bundle-widget tree"
  (let ((context (make-rendering-context))
	(widget (make-bundle-widget top-bundle system)))
    (configure-bundle-widget widget context stylesheet system)))

(defgeneric make-bundle-widget (bundle system)
  (:documentation "Turn a bundle for a presentation system which
  dictates default behaviors")
  (:method ((bundle bundle) system)
    (error "Bundle-widget is an abstract class and should not be instantiated
            by ~A for system ~A" bundle system)))

(defgeneric configure-bundle-widget (bundle-widget context stylesheet system)
  (:documentation "Configure a bundle widget given context, stylesheet and system")
  (:method ((widget bundle-widget) context stylesheet system)
    "Default behavior is to cache translatable values and configure
     our children widgets"
    (cache-slot-translations widget)
    (mapcar (lambda (child)
	      (configure-bundle-widget child 
	       (update-context bundle-widget context system)
	       stylesheet system))
	    (widget-children widget))))


;;
;; PAGE LAYOUTS
;;

(defwidget page-widget ()
  ((header-p :accessor render-header-p :initarg :header-p :initform t)
   (help-p :accessor render-header-help-p :initarg :help-p :initform nil)))

;; Should we default to a 1:1 assumption of bundles to containers?
;; Some of this should be factored out to the base class
(defmethod make-bundle-widget ((bundle page-bundle) system)
  "Mostly to select the widget instance we need"
  (let ((widget (make-instance 'page-widget :bundle bundle)))
    (setf (widget-children widget)
	  (mapcar (f (bundle) (make-bundle-widget bundle system))
		  (children bundle)))))

(defmethod configure-bundle-widget ((widget page-widget) context stylesheet system)
  "Placeholder example: add stylesheet support for help state, etc?"
  (when (> (context-depth context) 0)
    (setf (render-header-p widget) nil))
  (call-next-method))

(defmethod render-widget-body ((widget page-widget) &rest args)
  "This renders the page header.  Weblocks by default will then
  render all of the children bundle-widgets inside the overall
  widget header."
  (declare (ignore args))
  (when (render-header-p widget)
    (with-html
      (:div :class "page-widget-header"
	    (:span :class "page-widget-title"
		   (get-cached-slot-translation widget 'title))
	    (:span :class "page-widget-description"
		   (get-cached-slot-translation widget 'description))
	    (:span :class "page-header-help-link"
		   (render-link (f* (toggle-slot widget 'help-p)) 
				"Help..."))
	    (when (render-header-help-p widget)
	      (htm (:span :class "page-widget-help"
			  (get-cached-slot-translation widget 'help))))))))

;;
;; QUESTIONS (as presentation as widgets)
;;

(defmethod make-bundle-widget ((bundle question))
  (make-presentation question (current-id)))

(defmethod configure-bundle-widget ((p web-field-presentation)
				    context stylesheet system)
  (declare (ignore context stylesheet system))
  (when (equal (type-of (context-parent context)) 'row-bundle-widget)
    ;; set inhibit prompt p
    nil))

;;
;; Presentations as widgets
;;

(defmethod render-widget-body ((p web-field-presentation) &rest args)
  "Support basic widget protocol for presentations"
  (declare (ignore args))
  (render-presentation p))

(defmethod render-widget-children ((p web-field-presentation) &rest args)
  "Support basic widget protocol for presentations"
  (declare (ignore args))
  nil)


;;
;; UTILITIES
;;  

(defun toggle-slot (object slotname)
  (setf (slot-value object slotname)
	(not (slot-value object slotname))))

;;
;; Cheap value cache
;;

(defun cache-bundle-value ((widget bundle-widget) key text)
  (assoc-setf (value-cache widget) key text))

(defun get-bundle-value ((widget bundle-widget) key)
  (assoc-get (value-cache widget) key))

;;
;; Special case translation slot values
;;
  
(defmethod cache-slot-translations ((widget bundle-widget) &optional slotnames)
  (mapcar (lambda (slotname)
	    (cache-bundle-value widget slotname 
				(slot-value-translation (bundle widget) slotname)))
	  (or slotnames (translate-fields (bundle widget)))))

(defmethod get-cached-slot-translation ((widget bundle-widget) slotname)
  (aif-ret (get-bundle-value widget slotname)
    (slot-value-translation (bundle widget) slotname)))
       



