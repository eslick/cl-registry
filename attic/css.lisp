(in-package :registry)

;; connecting css to other objects that may be rendered

;; top level css support

(defvar *css-defs* (make-hash-table))

(defmacro define-css-rules (name &body rules)
  `(progn
     (setf (gethash ,name *css-defs*)
	   (render-css-to-string ,rules))))

(defun render-css (rulespecs)
  "Renders css inside a style tag (using weblocks/cl-who format)"
  (weblocks:with-html
    (:style :type "text/css"
	    (str #\Newline)
	    (str "<!--")
	    (str #\Newline)
	    (str (generate-css-string rulespecs))
	    (str "-->")
	    (str #\Newline))))

(defun render-css-to-string (rulespecs)
  "Renders an inline css to a string for later sending"
  (let ((*weblocks-output-stream* (make-string-output-stream)))
    (declare (special *weblocks-output-stream*))
    (render-css rulespecs)
    (get-output-stream-string *weblocks-output-stream*)))


;; Generating files

(defmacro define-css-file (filename &rest rulespecs)
  (with-gensyms (rspecs fname)
    `(let ((,rspecs ',rulespecs)
	   (,fname ,filename))
       (setf (gethash ,filename *css-files*) rspecs)
       (generate-css-file ,filename ,rspecs))))

(defmacro extend-css-file (filename &rest rulespecs)
  (with-gensyms (rspecs fname)
    `(let ((,rspecs ',rulespecs)
	   (,fname ,filename))
       (setf (gethash ,filename *css-files*) rspecs)
       (generate-css-file-extension ,filename ,rspecs))))

(defun generate-css-file (filename rulespecs)
  (with-open-file (stream filename :direction :output :if-exists :supersede :if-does-not-exist :create)
    (generate-css stream rulespecs)))

(defun generate-css-file-extension (filename rulespecs)
  (with-open-file (stream filename :direction :output :if-exists :append :if-does-not-exist :create)
    (format stream "~%~%/* This is auto-generated CSS - edit the lisp source */~%")
    (generate-css stream rulespecs)))

;; Main generator interface

(defun generate-css-string (rulespecs)
  (with-string-stream (str)
    (generate-css str rulespecs)))

(defun generate-css (stream rulespecs)
  (loop for rule in rulespecs do
       (let ((css-rule (make-css-rule (first rule) (rest rule))))
	 (format stream "~A {~%~{~A;~%~}}~%~%"
		 (string-join (css-rule-selectors css-rule) ",")
		 (loop for propval on (css-rule-properties css-rule) by #'cddr
		    collect (concatenate 'string "   " (propval-to-string propval)))))))

;; Rules


(defun make-css-rule (selectors properties)
  (list (mapcar #'val-to-string
		(if (atom selectors)
		    (list selectors)
		    selectors))
	properties))

(defun css-rule-selectors (css-rule)
  (first css-rule))

(defun css-rule-properties (css-rule)
  (second css-rule))

(defun propval-to-string (propval)
  (format nil "~A:~A" (val-to-string (first propval))
	  (val-to-string (second propval))))

;; css utils

(defun val-to-string (val)
  (cond ((stringp val) val)
	((symbolp val) (string-downcase (symbol-name val)))
	((consp val) (extract-list-vals val))
	(t (princ-to-string val))))

(defun extract-list-vals (list)
  (with-output-to-string (str)
    (mapcar (lambda (val)
	      (write-string (val-to-string val) str)
	      (write-char #\Space str))
	    list)))

(defun string-join (strings elt)
  (list-to-string (list-join strings elt)))

(defun list-join (list elt)
  (let (res)
    (dolist (i list)
      (push i res)
      (push elt res))
    (pop res)
    (nreverse res)))

