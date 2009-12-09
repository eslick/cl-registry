(in-package :registry)

(registry-proclamations)

;; =========================================================
;;  A dynamically adaptive navigation widget
;; =========================================================

(defwidget dynamic-navigation (selector)
  ((panes :accessor dynamic-panes
	  :initarg :panes
	  :initform nil
	  :documentation "A list of dynamic pane records")
   (permanent-panes :accessor permanent-panes
		    :initarg :permanent-panes
		    :initform nil
		    :documentation "A list of permanent pane records")
   (current-pane :accessor current-dynamic-pane
		 :initarg :current
		 :initform nil
		 :documentation "The current pane record")
   (create-fn :accessor dynamic-pane-create-fn
	      :initarg :create-fn
	      :initform nil
	      :documentation "When we get tokens that don't match an existing frame, 
                              call this fn with nav and uri-tokens; returns a pane rec
                              and modifies uri-tokens as appropriate")
   (callbacks :accessor callbacks
	      :initarg :callbacks
	      :initform (make-hash-table)
	      :documentation "Keys are pane refs (typically a model
	      object), values are lists of functions to be called when
	      the pane is about to be removed.  The functions receive
	      the pane ref an an argument.")))

(defmethod dependencies append ((navigation dynamic-navigation))
  (list (make-local-dependency :stylesheet "navigation")))

;; =========================================================
;;  Keeping track of panes
;; =========================================================

(defstruct dynamic-pane ref title tokens widget)

(defmethod print-object ((dp dynamic-pane) stream)
  (format stream "#S<DYN-PANE '~A' ~A>" 
	  (or (dynamic-pane-title dp)
	      (dynamic-pane-ref dp))
	  (dynamic-pane-tokens dp)))

(defun pane-by-reference (nav ref)
  (find ref (dynamic-panes nav) :test #'equal :key #'dynamic-pane-ref))

(defun pane-by-title (nav title)
  (find title (dynamic-panes nav) :test #'equal :key #'dynamic-pane-title))

(defun pane-by-tokens (nav tokens)
  (find tokens (dynamic-panes nav) :test #'equalp :key #'dynamic-pane-tokens))

(defun pane-by-uri-tokens (nav uri-tokens)
  "Given a uri-tokens object, find the subsequence of tokens matching an existing pane"
  (let ((tokens (remaining-tokens uri-tokens)))
    (loop for i from 1 upto (length tokens)
       for pane = (pane-by-tokens nav (subseq tokens 0 i))
       when pane do (return pane))))

;; =========================================================
;;  Dispatching 
;; =========================================================

(defmethod get-widget-for-tokens ((nav dynamic-navigation) uri-tokens)
  "Returns the pre-existing nav element or calls a function which
   returns consumed-tokens, widget, title, focus-p, and an optional reference object
   friendly-url-handler is provided"
  (aif (pane-by-uri-tokens nav uri-tokens)
       (progn 
	 (pop-tokens uri-tokens (length (dynamic-pane-tokens it)))
	 (setf (current-dynamic-pane nav) it)
	 (dynamic-pane-widget it))
       (awhen (dynamic-pane-create-fn nav)
	 (mvbind (widget consumed title ref focus-p add-p) (funcall it nav uri-tokens)
	    (pop-tokens uri-tokens (length consumed))
	    (if add-p
		(add-pane nav widget (mklist consumed) ref :title title :focus-p focus-p)
		(if focus-p 
		    (select-pane nav (pane-by-reference nav ref))
		    (aif (current-dynamic-pane nav)
			 (dynamic-pane-widget it)
			 (select-pane nav (first (dynamic-panes nav))))))))))

;; =========================================================
;;  Doing navigation pane flows
;; =========================================================

(defun/cc do-pane (nav ref callee tokens &optional title)
   (prog1
       (weblocks::call callee
 		      (lambda (new-callee)
			(add-pane nav new-callee (mklist tokens) 
				  ref :title title)))
     (remove-pane nav (pane-by-reference nav ref))))

;; =========================================================
;;  Adding and deleting panes
;; =========================================================

(defmethod select-pane ((nav dynamic-navigation) pane)
  (unless (eq pane (current-dynamic-pane nav))
;;    (setf (widget-children nav :selector) (dynamic-pane-widget pane))
    (setf (current-dynamic-pane nav) pane)))

;;(defmethod (setf current-dynamic-pane) :after (pane nav)
;;  (setf (widget-children nav :selector) (dynamic-pane-widget pane)))

(defmethod add-pane ((nav dynamic-navigation) widget tokens ref 
		     &key title permanent-p (focus-p t) (redirect-p t))
  "Add a pane to a dynamic navigation widget.  Keyword argument permanent
   means the pane is permanent and cannot be removed"
  (unless title (setf title (humanize-name ref)))
  (let ((existing (or (and ref (pane-by-reference nav ref))
		      (and tokens (pane-by-tokens nav tokens)))))
    (if existing
	(when focus-p 
	  (select-pane nav existing))
	(let ((pane (make-dynamic-pane :ref ref :tokens tokens :widget widget :title title)))
	  (weblocks::push pane (dynamic-panes nav))
	  (when permanent-p (push pane (permanent-panes nav)))
	  ;;      (mark-dirty nav) ;; re-render menu and/or main pane!
	  (when focus-p 
	    (select-pane nav pane)))))
  (when redirect-p
    (redirect-dynamic-nav nav (current-dynamic-pane nav)))
  (awhen (current-dynamic-pane nav)
    (dynamic-pane-widget it)))


(defmethod remove-pane ((nav dynamic-navigation) pane &key (redirect-p t))
  "Remove a pane from a dynamic navigation"
  (when (member pane (permanent-panes nav))
    (error "Cannot remove permanent pane with ref ~A from ~A" 
	   (dynamic-pane-ref nav) nav))
  (handle-callbacks nav pane)
  (when (eq pane (current-dynamic-pane nav))
    (let* ((old-pos (position pane (dynamic-panes nav)))
	   (new-pos (if (= old-pos 0) 1 (1- old-pos))))
      (setf (current-dynamic-pane nav)
	    (nth new-pos (dynamic-panes nav)))))
  (setf (dynamic-panes nav)
	(remove pane (dynamic-panes nav)))
  (when redirect-p
    (redirect-dynamic-nav nav (current-dynamic-pane nav)))
  (awhen (current-dynamic-pane nav)
    (dynamic-pane-widget it)))

(defun redirect-dynamic-nav (nav pane)
  (post-action-redirect (pane-uri nav pane)))

(defun pane-uri (nav pane)
  (concatenate 'string
	       (selector-base-uri nav)
	       "/"
	       (string-left-trim 
		"/"
		(string-right-trim
		 "/"
		 (uri-tokens-to-string (dynamic-pane-tokens pane))))))

;; =========================================================
;;  Callbacks on pane deletion based on reference object
;; =========================================================

(defmethod register-on-close-callback ((nav dynamic-navigation) ref fn)
  "Arrange to call FN with REF as an argument when the pane
corresponding to REF is removed."
  (push fn (gethash ref (callbacks nav))))

(defun handle-callbacks (nav pane)
  (let ((ref (dynamic-pane-ref pane)))
    (dolist (fn (gethash ref (callbacks nav)))
      (funcall fn ref)))
  (remhash pane (callbacks nav)))

;; ============================================================
;;  Rendering a dynamic menu
;; ============================================================

(defmethod render-widget-body ((obj dynamic-navigation) &rest args)
  (apply #'render-navigation-menu obj args))

(defmethod render-widget-children ((obj dynamic-navigation) &rest args)
  (with-html 
    (:div :class "navigation-body"
	  (mapc (f_ (apply #'render-widget _ args))
		(widget-children obj :selector)))))

(defmethod render-navigation-menu ((nav dynamic-navigation) &rest args)
  (let ((selected-pane (current-dynamic-pane nav))
	(panes (dynamic-panes nav))
	(container-id (gen-id)))
    (with-html 
      (str (format nil "<div style=\"height:~Apx;\">" (* 29 (ceiling (/ (length panes) 3))))))
    (labels ((render-panes ()
	       (dolist (pane (reverse panes))
		 (render-pane pane)))
	     (render-pane (pane)
	       (with-html
		 (:li :id (weblocks::unattributized-name (format nil "~A-~A" container-id 
								 (last1 (dynamic-pane-tokens pane)))
							 'menu-item)
		      :class (when (eq pane selected-pane) "selected-item")
		      (:span :class "item-wrapper"
			     (dynamic-nav-button-renderer nav pane args))))))
      (with-html
	(:div :class "view menu"
	      :id (weblocks::unattributized-name container-id 'menu)
	      (if (null panes)
		  (htm
		   (:div :class "empty-menu" "No navigation entries"))
		  (htm 
		   (:ul (render-panes)))))
	(str "</div>")
	(:hr :style "color: grey;")))))

(defun dynamic-nav-button-renderer (nav pane args)
  (declare (ignore args))
  (with-html 
    (:span :class "dyn-nav-menu-entry"
	   (htm (:a :class "dyn-nav-menu-text"
		    :href (pane-uri nav pane)
		    (str (dynamic-pane-title pane))))
	   (unless (member pane (permanent-panes nav))
	     (render-image-link (f_% (remove-pane nav pane))
				"/pub/images/nav/action_delete.gif"
				:text "close"
				:alt "close window")))))

;; (defun render-menu-button-url (nav target)
;;   (make-webapp-uri
;;    (string-left-trim
;;     "/" (concatenate 'string
;; 		     (string-right-trim "/" (selector-base-uri nav))
;; 		     "/"
;; 		     (typecase target
;; 			 (string (string-right-trim "/" target))
;; 			 (list (weblocks::uri-tokens-to-string target)))))))

	    
