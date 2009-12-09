(in-package :registry)

(defun make-draggable-list (id items)
  (make-instance 'draggable-list
		 :dom-id id
		 :items items))

(defwidget draggable-list (widget)
  ((items :initform nil :initarg :items :accessor draggable-list-items)
   (ajax-data :initform "AJAX DATA" :initarg :result :accessor draggable-list-ajax-data)))

(defmethod dependencies append ((w draggable-list))
  (list (make-local-dependency :script "draggable-list")
	(make-local-dependency :script "my-weblocks")))

;; ===================================
;;  Rending the body
;; ===================================

(defmethod render-widget-body ((w draggable-list) &rest args)
  (declare (ignore args))
  (format t "Rendering ~A~%" (dom-id w))
  (let ((code (make-action (lambda (&key list &allow-other-keys)
			     (sortable-update-handler w list)))))
    (with-html
      (:p (str (draggable-list-ajax-data w)))
      ;; Make sure our internal object has a canonical id name
      ;; Items also implement an ID function which defaults to prefix 'item_'
      (:ol :id (sortable-dom-id w)
	   (dolist (item (draggable-list-items w))
	     (htm
	      (:li :id (sort-item-dom-id item)
		   (str (group-name item))))))
      ;; The hidden anchor stores the action code for javascript use
      (:a :id (handler-dom-id w)
	  :handler code))))

(defmethod sort-item-dom-id ((object t))
  (format nil "item_~d" (object-id object)))

(defmethod sortable-dom-id (w)
  (concatenate 'string (dom-id w) "-sortable"))

(defmethod handler-dom-id (w)
  (concatenate 'string (dom-id w) "-handler"))


;; ===================================
;;  Handling new sort orders
;; ===================================

(defun sortable-update-handler (w sortable-string)
  "The main drag & drop action"
  (declare (special weblocks::*on-ajax-complete-scripts*))
  (let ((new-order (parse-sortable-output sortable-string)))
    ;; Debugging
    (setf (draggable-list-ajax-data w) new-order) 
    ;; Set new sorted order
    (setf (draggable-list-items w) 
	  (re-sort-list (draggable-list-items w) new-order)))
  ;; Make sure when we re-render that the parent widget
  ;; recreates it's sortable.  Don't do this if we don't re-render
  ;; on sort updates.  This is a hack for now, we'll fix it later.
  (push (weblocks::json-function 
	 (format nil "initializeDraggableList('~A')" (dom-id w)))
	*on-ajax-complete-scripts*))

(defun re-sort-list (old-list new-order)
  "Use new-order to construct a fresh list by picking elements from
   the old list"
  (mapcar #'(lambda (id)
	      (find id old-list :key #'object-id))
	  new-order))

(defun parse-sortable-output (string)
  "Extract lisp integers from the Sortable serializer output"
  (mapcar #'(lambda (string)
	      (read-from-string
	       (subseq string 1)))
	  (cl-ppcre:all-matches-as-strings "=(\\d+)[^&]" string)))


;; If we need to update the widgets:
;;
;;  (push (weblocks::json-function 
;;	 "initializeDraggable('draglistol')")
;;	 *on-ajax-complete-scripts*))


;;  If we don't need to update the widget we can inhibit rendering:
;;  
;;  (setf *dirty-widgets* (remove w *dirty-widgets*)))
