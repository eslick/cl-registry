(in-package :registry)

;;
;; Simple public page
;;

(defwidget front-page-home (selector)
  ((article-widget :accessor article-widget 
		   :initform (make-tabbed-articles-widget "tour")))
  (:default-initargs :dom-id "content"))

(defmethod get-widget-for-tokens ((page front-page-home) uri-tokens)
  (let ((tokens (remaining-tokens uri-tokens)))
    (if (and tokens (= (length tokens) 1))
	(progn
	  (setf (articles-page-name (article-widget page)) (first tokens))
	  (values page tokens nil))
	(values page tokens nil))))

(defmethod render-widget-body ((page front-page-home) &rest args)
  (declare (ignore args))
  (render-widget (article-widget page))
  (with-html (:div :id "front-page-end")))

(defmethod dependencies append ((page front-page-home))
  (list (make-local-dependency :stylesheet "front")))
