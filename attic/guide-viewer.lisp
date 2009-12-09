(in-package :registry)

(defwidget guide-viewer ()
  ((guide :accessor viewer-guide :initarg :guide :initform nil)
   (current-node :accessor viewer-node :initarg :node :initform nil)
   (toc :accessor viewer-toc :initarg :toc :initform nil)))

(defmethod initialize-instance :after ((viewer guide-viewer) &rest initargs)
  (declare (ignore initargs))
  (awhen (viewer-guide viewer)
    (setf (viewer-toc viewer) (compute-toc (viewer-guide viewer)))))

(defmethod render-widget-body ((viewer guide-viewer) &rest args)
  (declare (ignore args))
  (with-html
    (:div :class "boxed toc"
	  (render-toc (viewer-toc viewer))))
    (:div :class "bigdoc"
	  (render-nav (viewer-node viewer)))
    (render-node viewer (viewer-node viewer)))

(defun render-toc (viewer)
  (with-html 
    (:ul :class "bigdoc_toc"
	 )))

(defmacro with-node-nav ((up prev next) viewer)
  (assert (symbolp viewer))
  (with-gensyms (guide toc)
    `(let ((,guide (viewer-guide ,viewer))
	   (,toc (viewer-toc ,viewer))
	   (,node (viewer-node ,viewer)))


(defun render-nav (viewer node)
  (with-node-nav (up prev next) viewer
    (with-html
      (:div :class "bigdoc_nav boxed"
	    (:div :class "corners"
		  (:div :class "tl")
		  (:div :class "tr"))
	    (:div :class "bigdoc_crumbs"
		  (:div :class "bigdoc_crumb"
			(:a :href "/" "Top"))
		  (:div :class "bigdoc_crumb"
			(:span (str (title node)))))
	    (:div :class "bigdoc_next_prev"
		  (:div :class "bigdoc_pref"
			(:a :href "/" "&lt;"
			    (str (node-prev viewer node))))
		  (:div :class "bigdoc_next"
			(:a :href "/" 
			    (str (node-next viewer node)) 
			    " &gt;"))
		  (:div :style "clear:left"))
	    (:div :class "corners"
		  (:div :class "bl")
		  (:div :class "br")))
    (:

(defun node-prev (viewer node)
  (find-toc-node


(defun render-node (viewer node)		
  (let ((node-id (as-id (guide-title node))))
    (div :class "node"
	 (:p (:a :name node-id
		 :id node-id 
		 "Next:&nbsp;"
		 (render-link (lambda (x) 
				(setf (guide-node 
		 (:a :rel "next"
		     :accesskey "n"
		     :href "Pebbles.html#Pebbles">Pebbles</a>, Previous:&nbsp;<a rel="previous"
                   accesskey="p"
                   href="index.html#Top">Top</a>, Up:&nbsp;<a rel="up"
                   accesskey="u"
                   href="index.html#Top">Top</a>
            </p>

            <hr />
        </div>


(defun as-id (string)
  (substitute #\- #\Space (copy-seq string)))