(in-package :registry) 

(registry-proclamations)

(defwidget articles (dataseq)
  ((name :accessor articles-page-name :initarg :name)
   (title-p :accessor title-p :initarg :title-p :initform t)
   (sidebar-p :accessor sidebar-p :initarg :sidebar-p :initform nil)
   (standalone-p :accessor standalone-p :initarg :standalone-p :initform nil)
   (answer-p :accessor answer-p :initarg :answer-p :initform nil))
  (:documentation "Provides a widget that allows users navigation among a set of
     pages that are stored in database objects in the markdown format.  There is
     a companion article-editor which allows users to add markdown content and
     create new pages"))

(defun make-article-widget (pagename &key render-title-p sidebar-p standalone-p render-answer-p dom-id)
  (make-instance 'composite :widgets
		 (list (make-instance 'articles
				      :name pagename
				      :sidebar-p sidebar-p
				      :standalone-p standalone-p
				      :title-p render-title-p
				      :answer-p render-answer-p
				      :data-class 'article
				      :dom-id dom-id))))

(defun make-article-page (pagename &optional sidebar-p)
  (make-article-widget pagename :sidebar-p sidebar-p))

(defun page-articles (widget)
;;  (declare (ignore sort page-range))
  (let ((pagename (or (request-parameter "page")
		      (articles-page-name widget))))
    (articles-for-pagename pagename (dataseq-class-store widget))))

(defun articles-for-pagename (pagename &optional (store *registry-main*))
  (select-if (lambda (obj)
	       (equal (article-page obj) pagename))
	     (find-persistent-objects store
	      'article
	      :order-by '(order . :asc))))

(defmethod render-widget-body ((widget articles) &rest args)
  (declare (ignore args))
  (labels ((render-articles ()
	     (mapc (curry2 'render-article (title-p widget)) 
		   (page-articles widget))
	     (when (answer-p widget)
	       (with-html
		 (:br) 
		 (:p (render-link (f* (answer widget)) "Return"))
		 (:br))))
	   (div-style-for-articles ()
	     (cond
	       ((sidebar-p widget) "articles-page-sidebar")
	       ((standalone-p widget) "articles-page-standalone"))))
    (aif (div-style-for-articles)
	 (with-html
	   (:div :class it (render-articles)))
	 (render-articles))))

(defun render-article (article &optional title-p)
  (with-html 
    (:div :class "article"
	  (when title-p
	    (htm (:h1 (str (slot-value-translation article 'title)))))
	  (:div :class "article-body" 
		(str (maybe-markdown
                      (slot-value-translation article 'content)
                      (article-content-type article)))))))

;;
;; Article dialog
;;

(defun make-article-dialog-widget (pagename &optional 
				   (prompt-type :ok)
				   render-title-p)
  (make-instance 'composite :widgets
		 (list (make-instance 'articles-dialog-widget
				      :name pagename
				      :prompt-type prompt-type
				      :title-p render-title-p
				      :data-class 'article))))

(defwidget articles-dialog-widget (articles)
  ((prompt-type :accessor prompt-type :initarg :prompt-p 
		:initform :ok)))

(defmethod render-widget-body ((widget articles-dialog-widget) &rest args)
  (declare (ignore args))
  (with-html
    (:div :class "articles-dialog-body"
	  (call-next-method))))


(defmethod render-widget-body :after ((widget articles-dialog-widget) &rest args)
  "Renders prompt type of :ok :ok-cancel :yes-no or nil"
  (declare (ignore args))
  (when (prompt-type widget)
    (with-html-form (:post (lambda (&key ok yes &allow-other-keys)
			     (answer widget (or ok yes))))
      (case (prompt-type widget)
	(:ok (render-translated-button "OK" nil))
	(:ok-cancel (render-translated-button "OK" nil)
                    (render-translated-button "Cancel"))
	(:yes-no (render-translated-button "Yes")
                 (render-translated-button "No"))))))



;;
;; Provide a menu of the ordered articles to select from
;;

(defun make-tabbed-articles-widget (pagename)
  (make-instance 'tabbed-articles
		 :name pagename
		 :data-class 'article))

(defwidget tabbed-articles (articles)
  ((current-article :accessor current-article)))

(defmethod initialize-instance :after ((widget tabbed-articles) &rest initargs)
  (declare (ignore initargs))
  (assert (articles-page-name widget))
  (setf (current-article widget)
	(first (page-articles widget))))

(defmethod render-widget-body ((widget tabbed-articles) &rest args)
  (declare (ignore args))
  (let ((articles (page-articles widget)))
    (with-html
      (:ul :class "articles-menu"
       (dolist (article articles)
	 (if (eq article (current-article widget))
	     (htm (:li :class "articles-menu-item current-article"
		       (:p (str (slot-value-translation article 'title)))))
	     (htm (:li :class "articles-menu-item"
		       (render-link 
			(f* (setf (current-article widget) article))
			(slot-value-translation article 'title)))))))
      (aif (current-article widget)
	   (render-article it)
	   (htm "")))))
    
