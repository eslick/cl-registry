(in-package :registry)

(registry-proclamations)

;;
;; Various helpful utilities
;;

(defun get-object (id)
  (elephant::controller-recreate-instance *store-controller* id))

(defmacro assert-body-type (type &body body)
  (with-gensyms (value)
    `(let ((,value (progn ,@body)))
       (assert (eq (type-of ,value) ,type))
       ,value)))

;;
;; Find a natural split point for NL text content
;;

(defparameter *max-content-unit-length* 256)

(defun split-content (content &optional (length *max-content-unit-length*))
  (if (equal content "")
      nil
      (multiple-value-bind (end start)
	  (find-split-point content length)
	(format t "end: ~A start: ~A~%" end start)
	(cond ((and (eq end 0) (< (length content) 1))
	       nil)
	      ((eq end 0)
	       (split-content (subseq content (1+ start))))
	      (t
	       (cons (subseq content 0 end)
		     (split-content (subseq content start))))))))

(defun find-split-point (content max)
  (let ((sentence-end (position-if (lambda (char)
				     (member char '(#\! #\. #\? #\;)))
				   content :from-end t :end (min (length content) max)))
	(newline (position #\Newline content :end max)))
    (format t "l: ~A s:~A n:~A~%" (length content) sentence-end newline)
    (cond ((and newline (< newline (or sentence-end max)))
	   (let ((last-newline (skip-newlines content newline)))
	     (values newline last-newline)))
	  (sentence-end 
	   (let ((end (min (+ sentence-end 2) (length content))))
	     (values end end)))
	  ((< (length content) max)
	   (values (length content) (length content)))
	  (t
	   (let ((pos (or (position #\Space content :from-end t :end max) 
			  (- (length content) 2))))
	     (print pos)
	     (values (1+ pos) (1+ pos)))))))

(defun skip-newlines (string offset)
  (or (loop for i from offset to (length string) do
	   (when (not (eq (char string i) #\Newline))
	     (return (1- i))))
      offset))


;;
;; Little list utilities (used in group.lisp)
;;

(defun nsplice-list-after (list inserted element &key (test 'equal))
  (aif (member element list :test test)
       (progn (rplacd it (append inserted (rest it)))
	      list)
       (append inserted list)))

(defun move-list-element (list element insert-after &key (test 'equal))
  (assert (not (some #'null list)))
  (let ((position (if (null insert-after) 0
		      (position insert-after list :test test))))
    (assert (numberp position))
    (ninsert (remove element list) element position)))

(defun list-prev (ref list &aux last-one)
  "Return the element before elt in list"
  (loop for elt in list do
       (if (equal elt ref)
	   (return last-one)
	   (setf last-one elt))))

(defun list-next (ref list)
  "Return the element after elt in list"
  (awhen (cdr (member ref list))
    (car it)))

;;
;; Shadow with html to allow *dev
;;

(defvar *dev-mode* t)

(defmacro with-html (&body body)
  `(cl-who:with-html-output (*weblocks-output-stream* nil :indent *dev-mode*)
     ,@body))

(defmacro with-return-it (expr &body body)
  `(let ((it ,expr))
     (progn
       ,@body
       it)))
     
;;
;; Some rendering aids
;;

(defun render-text-input (name value &key id (class "text") maxlength (style ""))
  "Renders a text input.
'name' - name of the html control. The name is attributized before being rendered.
'value' - The value.
'id' - id of the html control. Default is nil
'maxlength' - Maximum length of the field'
'class' - css class. Defaults to \"text\"."
  (with-html
    (:input :type "text" :name (attributize-name name) :id id
            :value value :maxlength maxlength :class class
	    :style style)))

(defun as-string (obj)
  (typecase obj
    (string obj)
    (t (format nil "~A" obj))))

(defun as-keyword (obj)
  (typecase obj
    (string (intern obj :keyword))
    (symbol (intern (symbol-name obj) :keyword))))

(defun render-image-link (action image-url &key alt class id 
			  text text-location (ajaxp t) title)
  (let* ((action-code (weblocks::function-or-action->action action))
	 (url (make-action-url action-code)))
    (with-html
      (:a :id id :class class
	  :tabindex "100"
	  :title title
	  :href url :onclick (when ajaxp
			       (format nil "initiateAction(\"~A\", \"~A\"); return false;"
				       action-code (weblocks::session-name-string-pair)))
	  (str (when (and text (eq text-location :before)) text))
	  (:img :src image-url :alt alt)
	  (str (when (and text (eq text-location :after)) text))))
    (weblocks::log-link image-url action-code :id id :class class)))


;;
;; Efficient database ops
;;

(defun objects-exist-p (class slot value)
  (map-inverted-index (lambda (value oid)
			(declare (ignore value oid))
			(return-from objects-exist-p t))
		      class
		      slot
		      :oids t
		      :value value)
  nil)

(defun object-subset-count (class slot value)
  (let ((count 0))
    (map-inverted-index (lambda (value oid)
			  (declare (ignore value oid))
			  (incf count))
			class
			slot
			:oids t
			:value value)
    count))


;;
;; Special rendering support
;;

(defun my-selector (widget)
  (if (subtypep (type-of widget) 'selector)
      widget
      (my-selector (widget-parent widget))))

;;; The name could be better, and this should probably
;;; be someplace else.
(defun render-dispatched-link (widget tokens name &key id class)
  "Render a link at the widget's current URL + extra tokens"
  (let* ((link-uri (concatenate 'string
			     (selector-base-uri (my-selector widget))
			     "/"
			     (format nil "~{~a~^/~}" tokens))))
    (with-html
      (:a :id id :class class
	  :href link-uri
	  (str name)))))


(defun render-dispatched-action-link (action widget tokens name &key id class)
  (let* ((my-uri (selector-base-uri (my-selector widget)))
	 (action-code (weblocks::function-or-action->action action))
	 (link-uri (concatenate 'string
			     my-uri
			     "/"
			     (format nil "~{~a~^/~}" tokens)
			     "?" weblocks::*action-string* "="
			     (weblocks::url-encode 
			      (princ-to-string action-code)))))
    (with-html
      (:a :id id :class class
	  :href link-uri
	  (str name)))))




;;
;; Random crap
;;  

(defun nconsolidate-rows (rows column &optional subset)
  "Find duplicate rows and count them, overwrite count 
   in the 'column' column of the row and return the consolidated 
   list.  Subset indicates the bounds of a subseq to use to
   compare.  Probably shouldn't include the count column!"
  (flet ((compare-set (list) 
	   (if subset
	       (subseq list (car subset) (cdr subset))
	       list)))
    (let ((processed nil))
      (dolist (row rows)
	(let* ((compare (compare-set row))
	       (exists (find compare processed :test #'equal
			     :key #'compare-set)))
	  (if exists
	      (incf (nth column exists))
	      (let ((new-row (copy-list row)))
		(setf (nth column new-row) 1)
		(push new-row processed)))))
      (nreverse processed))))
  
(defun getf-all (plist symbol)
  "Quick hack to get one or more values from the same property.
   If only one, return it without a list.  Works for both 
   multiple and single choice presentation types."
  (labels ((getter (key value rest &optional results)
	     (when (eq symbol key)
	       (push value results))
	     (if (null rest)
		 (if (= 1 (length results))
		     (first results)
		     results)
		 (getter (car rest) (cadr rest) (cddr rest) results))))
    (when plist
      (getter (car plist) (cadr plist) (cddr plist)))))

(defun as-argument-keyword (string)
  (intern (substitute #\- #\Space (string-upcase string)) :keyword))

(defmacro f* (&rest body)
  (with-gensyms (args)
    `(lambda (&rest ,args)
       (declare (ignorable ,args))
       ,@body)))

(defun kintern (string-designator &rest format-control)
  (intern (if format-control
              (apply #'format nil (string string-designator) format-control)
              (string string-designator))
          :keyword))

(defmacro with-session-context (&body body)
  `(let ((hunchentoot::*session* *last-session*)
	 (weblocks::*current-webapp* (second weblocks::*active-webapps*)))
     (declare (special hunchentoot::*session* weblocks::*current-webapp*))
     ,@body))
	     