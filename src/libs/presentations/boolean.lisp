(in-package :registry)

;;;; * web presentations for boolean objects

(defclass boolean-presentation (web-field-presentation)
  ()
  (:default-initargs :lisp-value nil))

(define-lisp-value-getter boolean-presentation (client-value)
  (cond ((string= client-value "t") t)
	((string= client-value "f") nil)
	(t :none)))

(define-lisp-value-setter boolean-presentation (new-value client-value)
  (cond ((eq new-value :none)
	 "")
	((eq new-value :false)
	 "f")
	((null new-value)
	 "f")
	((eq new-value :true)
	 "t")
	((eq new-value t)
	 "t")
	(t "")))

;; (if (and new-value (not (eq new-value :false))) "t" "f"))

(defmethod render-presentation ((presentation boolean-presentation))
  (format t (if (lisp-value presentation)
                #!"Yes"
                #!"No")))

(defun upgrade-boolean-answers-from-keywords ()
    (mapcar (lambda (answer)
	      (when (eq (question-data-type (question answer)) :boolean)
		(when (eq (value answer) :true) (setf (value answer) t))
		(when (eq (value answer) :false) (setf (value answer) nil))))
     (get-instances-by-class 'answer)))


;;;; ** rendered as a checkbox

(defclass checkbox-boolean-presentation (boolean-presentation)
  ())

(define-lisp-value-getter checkbox-boolean-presentation (client-value)
  (cond ((string= client-value "t") t)
	(t nil)))

(defmethod render-presentation-editable ((presentation checkbox-boolean-presentation))
  (with-html
    (:input :name (attributize-name (query-name presentation))
            :type "checkbox"
            :id (dom-id presentation)
            :class (css-class presentation)
            :style (css-style presentation)
            :value "t"
            :onclick (on-change-validation presentation)
            :checked (lisp-value presentation))))

;;;; ** rendered as a drop down (this need to be localized)

(defclass dropdown-boolean-presentation (boolean-presentation)
  ())

(defmethod render-presentation-editable ((presentation dropdown-boolean-presentation))
  (render-dropdown (query-name presentation) (list (cons #!"Yes" "t") (cons #!"No" "f"))))
;;		   :welcome "an option"))

;;;; ** rendered as a radio (this need to be localized)

(defclass radio-boolean-presentation (boolean-presentation)
  ())

(defmethod render-presentation-editable ((presentation radio-boolean-presentation))
  (render-radio-buttons (query-name presentation) (list (cons #!"Yes" "t") (cons #!"No" "f"))
                        :selected-value (client-value presentation)))
