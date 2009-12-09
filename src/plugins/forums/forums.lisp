(in-package :registry)

(registry-proclamations)

;;
;; Forums plugin definition
;;

(define-plugin forums (site-app)
  "The create function accepts a list of plugins"
  :tab-name 'discuss
  :create 'make-forums-page)

(defun make-forums-page (&rest args)
  (declare (ignore args))
  (let* ((w (make-instance 'composite)))
    (setf (composite-widgets w) (list (make-instance 'forums-dispatcher)))
    w))

;;
;; 
;;

(defparameter *default-posts-per-page* 5)

(defwidget add-post-widget (widget)
  ())

(defwidget edit-post-widget (widget)
  ())

(defun wmd-dependencies ()
  (list (make-local-dependency :script "wmd/wmd")
	(make-local-dependency :script "wmd/wmd-base")
	(make-local-dependency :script "wmd/wmd-plus")
	(make-local-dependency :script "wmd/showdown")
        (make-local-dependency :stylesheet "wmd")))

(defview forum-post-add-view (:type form :caption "Add post")
  ;;title (textarea :rows 10 :cols 80)))
;;  (content :present-as (tinymce :rows 20 :cols 150)))
  (content :present-as (wmd :rows 20 :cols 150)))

(defview forum-post-edit-view (:type form :caption "Edit post")
  ;;title (textarea :rows 10 :cols 80)))
;;  (content :present-as (tinymce :rows 20 :cols 150)))
  (content :present-as (wmd :rows 10 :cols 150)))


(defmethod render-widget-body ((w add-post-widget) &rest args)
  (declare (ignore args))
  (with-html-form ("get" (make-action #'(lambda (&rest args)
				(declare (ignore args))
				(do-widget w
				  (make-quickform 'forum-post-add-view
					 :data (make-instance 'forum-post
                                                              :content-type :markdown)
					 :on-success
					 #'(lambda (qform temp)
					     (declare (ignore qform))
					     (add-post (widget-parent w) temp)))))))
    (:input :type "submit" :value #!"Add a new Post")))
;;   (render-link (make-action #'(lambda (&rest args)
;; 				(declare (ignore args))
;; 				(do-widget w
;; 				  (make-quickform 'forum-post-add-view
;; 					 :data (make-instance 'forum-post
;;                                                               :content-type :markdown)
;; 					 :on-success
;; 					 #'(lambda (qform temp)
;; 					     (declare (ignore qform))
;; 					     (add-post (widget-parent w) temp))))))
;; 	       #!"[add a post]"))

(defun/cc do-edit-post (post-widget post)
  (do-dialog "" 
    (make-quickform 'forum-post-edit-view
		    :data-class-name 'forum-post
		    :data post
		    :on-success (lambda (qform temp)
				  (declare (ignore qform temp))
                                  ;; Editing requires conversion to markdown
                                  (setf (post-content-type post) :markdown)
				  (mark-dirty post-widget))
		    :answerp t)))

(defwidget link-widget (widget)
  ((target :accessor link-target :initarg :target)
   (name :accessor link-name :initarg :name :initform "link")))

(defmethod render-widget-body ((w link-widget) &rest args)
  (declare (ignore args))
  (render-link (lambda (&rest args)
		 (declare (ignore args))
		 (do-widget w (link-target w)))
	       (link-name w)))

;; ==================================================
;;  Topic Widget
;; ==================================================

;;; In order to be able to run with-flow, etc. on widgets that
;;; are contained in the topic-widget, we need to inherit from
;;; composite so that widgets have parents, etc.
;;;
;;; We therefore need to keep our child widgets in the
;;; composite-widgets list, and we implement slot-accessors
;;; in terms of list operations, which is kind of gross.

(defwidget topic-widget (composite)
  (;; for easier access to child widgets stored in composite-widgets
   ;; list
   (flash)
   (pagination)
   (add-post-widget)
   (post-widgets)
   (help-widget :accessor help-widget :initarg :help-widget
		:initform (make-instance 'quick-help :page "forums-help" 
					 :link-title #!"Get help here"
					 :dialog-p t))
   ;; normal slots
   (topic :accessor topic :initarg :topic)
   (visible-posts :accessor topic-visible-posts :initarg :visible-posts
		  :initform nil)
   (translate-all-p :accessor translate-all-p :initarg :translate-all-p
		    :initform nil)
   (translation :accessor translation :initform nil)
   (editp :accessor editp :initarg :editp :initform nil))
  (:documentation "The topic widget displays a forum topic, contained
    widgets in a page view and all special actions available on it."))

;; Make topic-widget compatible with lots of the translation functions
;; originally written for post-widget
(defmethod post ((widget topic-widget))
  (topic widget))

(defmethod showp ((widget topic-widget))
  (translate-all-p widget))

(defmethod dependencies append ((widget topic-widget))
  (list* (make-local-dependency :stylesheet "forums")
         (wmd-dependencies)))

(defmethod topic-widget-flash ((w topic-widget))
  (first (composite-widgets w)))

(defmethod pagination ((w topic-widget))
  (second (composite-widgets w)))

(defmethod add-post-widget ((w topic-widget))
  (third (composite-widgets w)))

(defmethod post-widgets ((w topic-widget))
  (nthcdr 3 (composite-widgets w)))

;;
;; Computed accessors
;;

(defun topic-posts (widget)
  (when (topic widget)
    (sort (get-instances-by-value 'forum-post 'topic (topic widget))
	  #'>
	  :key #'post-datetime)))

(defun topic-posts-count (widget)
  (length (get-instances-by-value 'forum-post 'topic (topic widget))))

(defun topic-page-posts (widget)
  (multiple-value-bind (low high)
      (pagination-page-item-range (pagination widget))
    (subseq (topic-posts widget) low high)))

;;
;; Initialization
;;

(defun make-topic-widget ()
  (let* ((topic-widget (make-instance 'topic-widget))
	 (flash-widget (make-instance 'flash))
	 (pagination-widget (make-instance 'pagination))
	 (add-post-widget (make-instance 'add-post-widget)))
    (setf (composite-widgets topic-widget)
	  (list flash-widget
		pagination-widget
		add-post-widget))
    topic-widget))

(defmethod render-widget-children ((widget topic-widget) &rest args)
  (declare (ignore args))
  nil)

(defun configure-topic-pagination (widget)
  (let ((p (pagination widget)))
    (setf (pagination-on-change p) #'(lambda (&rest args)
				       (declare (ignore args))
				       (mark-dirty widget)))
    (setf (pagination-show-total-items-p p) t)
    (setf (pagination-total-items p) (topic-posts-count widget))
    (setf (pagination-items-per-page p) *default-posts-per-page*)))

(defwidget ratings-widget (widget)
  ((obj :accessor rating-widget-object :initarg :obj)
   (rating-count :type integer)
   (user :accessor rating-widget-user :initform (current-user))))

(defmethod render-widget-body ((widget ratings-widget) &rest args)
  (declare (ignore args))
  (with-slots (obj user) widget
    (let* ((ratings (user-ratings user))
	   (rating (get-value obj ratings)))
      (with-html
	(str #!"Do you like this topic?")
	(:span :class (if (> rating 0) "my-rating rating" "rating")
	       (render-link (make-action
			     (lambda (&rest args)
			       (declare (ignore args))
			       (rate obj user 1)
			       (mark-dirty widget)))
			    (format nil "~A (~d)" #!"Yes" (up-ratings obj))))
	(:span :class (if (< rating 0) "my-rating rating" "rating")
	       (render-link (make-action
			     (lambda (&rest args)
			       (declare (ignore args))
			       (rate obj user -1)
			       (mark-dirty widget)))
			    (format nil "~A (~d)" #!"No" (down-ratings obj))))))))

;;
;; Render main view
;;
 
(defmethod render-breadcrumbs ((w topic-widget) &rest args)
  (declare (ignore args))
  (let* ((topic (topic w))
	 (category (topic-category topic)))
    (with-html
      (:p (:a :href "/dashboard/discuss" 
	      (str #!"Forums"))
	  "&nbsp; > &nbsp;"
	  (:a :href (format nil "/dashboard/discuss/category/~d"
			    (mid category))
	      (str (category-name category)))
	  "&nbsp; > &nbsp;"
	  (:a :href (format nil "/dashboard/discuss/topic/~d"
			    (topic-number topic))
	      (str (topic-subject topic)))))))

(defmethod render-widget-body ((widget topic-widget) &rest args)
  (declare (ignore args))
  (let* ((topic (topic widget))
         (content (content-html topic))
         (markdown-p (eq (topic-content-type topic) :markdown)))
    (with-html
      (render-widget (topic-widget-flash widget))
      (:div :class "forum-topic"
	    (:div :class "forum-topic-main"
		  (render-breadcrumbs widget)
		  (:div :class "forum-topic-header"
			(:h1 (str (topic-subject topic)))
			(:div :class "post-user" (str #!"by: ")
			      (str (username (topic-owner topic))))
			(:div :class "post-datetime"
			      (str #!"Last updated: ")
			      (present-as-static 'datetime-presentation (topic-date-updated topic)))
			(:hr)
			(if markdown-p
			    ;; markdown includes <p>...</p>
			    (with-html (str content))
			    (with-html (:p (str content))))
			;;		  (render-widget (make-instance 'ratings-widget :obj topic))
			;;                (:div :class "clear-both")
			;;		  (:span :class "related-content"
			;;			 (str (topic-related-content topic)))
			;;		  (:div :class "clear-both")
			)
		  (when (eq (current-user) (topic-owner topic))
		    (with-html-form ("get" (f* (do-edit-topic widget topic)))
		      (:input :type "submit" :value #!"Edit this Topic")))
		  (:div :class "translation-button"
			(when (allow-translation-p widget)
			  (render-translated-title widget)
			  (render-translation-body widget)
			  (when (editp widget)
			    (htm (:br)))))
;;			  (render-link (f* * )  #!"[edit this post]")
;;			  (htm (:br))))
		  (render-widget (add-post-widget widget))
		  (render-posts widget (topic-page-posts widget)))
	    (:div :class "forum-topic-right"
		  (:div :class "top"
			(render-widget (help-widget widget))
			(:h2 "Forum Actions")
			(:div :class "translation-button"
			      (if (translate-all-p widget)
				  (render-link (f* (setf (translate-all-p widget) nil))
					       #!"Hide Translations")
				  (render-link (f* (setf (translate-all-p widget) t))
					       #!"Show Translations"))))
		  (:div :class "bottom"))
	    (:div :class "float-end"))

;;; ticket:65
		  ;; 					   #!"translate posts to"
		  ;; 					   (find-translation 
		  ;; 					    (language-name 
		  ;; 					     (session-language)))))))
      (when (pagination widget)
	(render-widget (pagination widget))))))

(defview forum-topic-edit-view (:type form :caption "Edit post")
  (subject)
  (content :present-as (wmd :rows 10 :cols 150)))

(defun/cc do-edit-topic (topic-widget topic)
  (do-dialog "" 
    (make-quickform 'forum-topic-edit-view
		    :data-class-name 'forum-topic
		    :data topic
		    :on-success (lambda (qform temp)
				  (declare (ignore qform temp))
                                  ;; Editing requires conversion to markdown
                                  (setf (topic-content-type topic) :markdown)
				  (mark-dirty topic-widget))
		    :answerp t)))

(defun add-post (topic-widget temp-post)
  (let ((pagination (pagination topic-widget))
	(post (persist-object *default-store* temp-post)))
    (incf (pagination-total-items pagination))
    (setf (pagination-current-page pagination) 1)
    (link-post topic-widget post)
    (record-event :new-forum-post post)))

;; =====================================================
;;  Post Widget
;; =====================================================

(defwidget post-widget (widget)
  ((post :accessor post :initarg :post :initform nil)
   (translation :accessor translation :initform nil)
   (showp :accessor showp :initarg :showp :initform nil)
   (editp :accessor editp :initarg :editp :initform nil)))

(defun render-posts (widget posts)
  (loop for post in posts do
       (render-widget (make-instance 'post-widget
				     :post post
				     :showp (translate-all-p widget)))))
      
(defmethod render-widget-body ((widget post-widget) &rest args)
  (declare (ignore args))
  (let* ((post (post widget))
	 ;;(user (current-user))
	 ;;(ratings (user-ratings user))
	 ;;(rating (get-value post ratings))
         )
    (with-html
      (:div :class "forum-post"
;; 	    (:h2 
;; 	     (when (is-admin-p) (str (format nil "#~d: " (post-number post))))
;; 	     (if (allow-translation-p widget)
;; 		 (render-translated-title widget)
;; 		 (str (post-title post))))
            (:p :class "post-user" (str #!"by: ")
		(str (username (post-owner post))))
            (:span :class "datetime"
		   (present-as-static 'datetime-presentation (post-datetime post)))
            (:hr)
            (:p :class "post-content" (str (content-html post)))
	    (when (eq (current-user) (post-owner post))
;;	      (render-link (f* (do-edit-post widget post))
;;			   #!"[edit this post]"))
	      (with-html-form ("get" (f* (do-edit-post widget post)))
		(:input :type "submit" :value #!"Edit this Post")))
            (when (allow-translation-p widget)
              (render-translation-body widget))
	    ))))

#+nil
(defun render-posts (widget posts)
  (declare (ignore widget))
  (dolist (post posts)
    (render-object-view post 'forum-post-normal-view)))

(defparameter *static-presentation-dispatch*
  (make-instance 'datetime-presentation)
  "Keep an object around so we can use the presentation logic to print our
   dates in the post-widget")

(defparameter *static-dropdown-presentation*
  (make-instance 'dropdown-presentation))

#|(defparameter *static-stars-presentation*
  (make-instance 'stars-presentation)) |#

;;
;; Post translation support
;;

(defun allow-translation-p (widget)
  (and (not (equal (original-language (post widget))
		   (session-language)))
       (supports-translation-p *translation-service*
			       (original-language (post widget))
			       (session-language))))

(defun post-translation-text (widget field)
  (awhen (post-widget-translation widget)
    (awhen (assoc field (translation-alist it))
      (cdr it))))

(defun post-widget-translation (widget &optional create)
  (aif-ret (translation widget)
    (setf (translation widget)
	  (get-translation (post widget) (session-language) create))))

(defun post-widget-force-autotranslate (widget)
  (let ((post (post widget)))
    (setf (translation-cache post)
	  (make-translation post (session-language)))
    (setf (translation widget) (translation-cache post))
    (mark-dirty widget)))

(defun set-post-translation-text (widget field value)
  (when (equal value "") (setf value nil))
  (let* ((translation (post-widget-translation widget t))
	 (alist (translation-alist translation))
	 (cell (assoc field alist)))
    (cond (cell (setf (cdr cell) value))
          (value (push (cons field value) alist)))
    (setf (translation-alist translation)
	  alist)))

(defsetf post-translation-text set-post-translation-text)

;;
;; Translation and control rendering
;;

(defmethod render-translated-title ((widget post-widget))
  (with-html
    (cond ((and (showp widget) (not (editp widget)))
	   (htm (str (post-translation-text widget 'title))))
	  ((editp widget)
	   (render-text-input "edit-title"
			      (post-translation-text widget 'title))))))

(defmethod render-translated-title ((widget topic-widget))
  (with-html
    (cond ((and (showp widget) (not (editp widget)))
	   (htm (:b (str (post-translation-text widget 'subject)))))
	  ((and (showp widget) (editp widget))
	   (render-text-input "edit-title"
			      (post-translation-text widget 'subject))))))

(defun render-translation-body (widget)
  (with-html
    (cond ((and (showp widget) (not (editp widget)))
	   (htm ;;(:p (str "Show translation here"))))
	    (:blockquote
	     (:p :class "translation-text"
		 (str (post-translation-text widget 'content))))))
	   ((and (showp widget) (editp widget))
	    (htm
	     (:p :class "translation-edit"
		 (render-textarea "edit-content"
				  (post-translation-text widget 'content)
				  15 60)))))
    (render-translation-actions widget)))

(defmethod render-widget-body :around ((widget post-widget) &rest args)
  (declare (ignore args))
  (if (not (editp widget))
      (call-next-method)
      (with-html-form (:post (lambda (&key cancel edit-title edit-content &allow-other-keys)
			       (unless cancel
				 (setf (post-translation-text widget 'title) edit-title)
				 (setf (post-translation-text widget 'content) edit-content)
				 (setf (translation-editedp (post-widget-translation widget)) t))
			       (setf (editp widget) nil)))
	(call-next-method))))

(defmethod render-widget-body :around ((widget topic-widget) &rest args)
  (declare (ignore args))
  (if (not (editp widget))
      (call-next-method)
      (with-html-form (:post (lambda (&key cancel edit-title edit-content &allow-other-keys)
			       (unless cancel
				 (setf (post-translation-text widget 'subject) edit-title)
				 (setf (post-translation-text widget 'content) edit-content)
				 (setf (translation-editedp (post-widget-translation widget)) t))
			       (setf (editp widget) nil)))
	(call-next-method))))

(defun render-translation-actions (widget)
  (with-html
    (cond ((and (showp widget) (editp widget))
 	   (htm (:span :class "post-action"
		       (render-translated-button "submit")
		       (str " &nbsp; ")
		       (:input :type "submit" :name "cancel" :value #!"Cancel"))))
	  ((and (showp widget)
		(not (awhen (post-widget-translation widget)
		       (translation-trustedp it))))
	   (when (null (post-translation-text widget 'content))
	     (with-html-form ("get" (f* (post-widget-force-autotranslate widget)))
	       (:input :type "submit" :value #!"Auto-translate")))
	   (with-html-form ("get" (f* (log-message :ajax :debug "Edit action!")
				      (setf (editp widget) t)))
	     (:input :type "submit" :value #!"Edit Translation"))))))

;; =====================================================
;;  Utility functions
;; =====================================================

(defun current-user (&optional nil-if-none-p)
  (if (and nil-if-none-p (not (boundp 'hunchentoot:*session*)))
      nil
      (authenticatedp)))

(defun link-post (widget object)
  "Add object to post list, fixup times, assign owner"
  (let ((topic (topic widget)))
    (setf (post-owner object) (current-user))
    (setf (post-datetime object) (get-universal-time))
    (setf (post-topic object) topic)
    (setf (topic-date-updated topic) (post-datetime object))
    (setf (post-number object) (next-post-number topic))))
;;    (answer widget)))



;; ======================================================
;;  Deal with displaying topics by category
;; ======================================================

(defun make-topic-list-widget (category)
  (make-instance 'composite :widgets
		 (make-category-widget category)))

(defun make-category-widgets ()
  (mapcar #'make-category-widget (all-categories nil)))

(defun make-category-widget (category)
  (make-instance 'category-widget :category category))

(defwidget category-widget ()
  ((category :accessor category :initarg :category :initform nil)))

(defmethod dependencies append ((widget category-widget))
  (list (make-local-dependency :stylesheet "forums")))
       
(defmethod render-breadcrumbs ((w category-widget) &rest args)
  (declare (ignore args))
  (with-html
    (:p (:a :href "/dashboard/discuss" 
	    (str #!"Forums"))
	"&nbsp; > &nbsp;"
	(:a :href (format nil "/dashboard/discuss/category/~d"
			  (mid (category w)))
	    (str (category-name (category w)))))))

(defmethod render-widget-body ((w category-widget) &rest args)
   (declare (ignore args))
   (render-breadcrumbs w)
   (render-one-category w (category w))
   (with-html
     (:div :id "create-topic"
	   (render-link (lambda (&rest args)
			  (declare (ignore args))
			  (do-widget w (make-quickform 'forum-topic-add-view
 						       :data-class-name 
 						       'forum-topic
 						       :on-success
 						       (lambda (qform temp)
 							 (add-topic qform temp 
								    (category w))))))
			#!"[Create a new topic]"))))

(defun render-one-category (widget category)
  (let ((topics (sort (get-instances-by-value 'forum-topic 'category category)
		      #'> :key #'topic-date-created)))
    (with-html
      (:h2 :class "category-name" (str (category-name category)))
      (:table :class "view"
       (if (null topics)
	 (with-html
	   (:tr
	    (:td :class "topic-name" :rowspan 2 "no topics")))
	 (dolist (topic topics)
	   (with-html
	     (:tr
	      (:td :class "topic-name"
		   (render-dispatched-link widget
					   (list "topic" (topic-number topic))
					   (topic-subject topic)))
	      (:td :class "topic-owner" 
		   (str (format nil #!"by '~A'"
				(and (topic-owner topic)
				     (username (topic-owner topic))))))
	      (:td :class "topic-posts"
		   (str (format nil #!"( ~d ~:*~[posts~;post~:;posts~] )" 
				(length (get-instances-by-value 'forum-post 'topic topic)))))
	      (:td :class "topic-updated"
		   (str (format nil "~A"
;;				#!"Last updated"
				(present-as-static 
				 'datetime-presentation 
				 (topic-date-updated topic) 
				 :show-date-p t :show-time-p nil))))))))))))


(defun get-widget-uri-tokens2 (widget &key (root (root-composite)))
  (when (not (eq widget root))
    (let ((parent (widget-parent widget)))
      (typecase parent
	(dispatcher (append (get-widget-uri-tokens2 parent) 
			    (awhen (car (disp-cached parent))
			      (if (listp it)
				  it
				  (list it)))))
	(t (get-widget-uri-tokens2 parent))))))



(defun add-topic (w temp category)
  (declare (ignore w))
  (let ((topic (persist-object *default-store* temp)))
    (setf (topic-number topic) (next-topic-number))
    (setf (topic-category topic) category)))

;; ================================================================
;; Display a list of categories
;; ================================================================

;; 1) Add a 'forums' home page to the discuss section which maps to
;; the list of categories.  We're going to filter this list based on
;; the self-reported role of the user (patient, etc.)

(defwidget category-list (datagrid)
  ())

(defview category-table-view (:type table)
  name
  (topics :reader 'category-count))

(defun category-count (category)
  (length (get-instances-by-value 'forum-topic 'category category)))

;; Assuming that the datagrid widget is an acceptable UI,
;; we can use :on-query to filter which categories are displayed
;; when we need to do that.
(defun make-category-list-widget ()
  (make-instance 'category-list
		 :data-class 'forum-category
		 :allows-drilldown-p t
		 :on-drilldown (cons :show-category 'show-category)
		 :view 'category-table-view))

(defun show-category (grid category)
  (declare (ignore grid))
  (redirect (format nil "/dashboard/discuss/category/~d" (mid category))))

;;; Dispatcher for the forums

(defwidget forums-dispatcher (dispatcher)
  ((topic-widget :accessor topic-widget
		 :initform (make-topic-widget))
   (topic-list-widget :accessor topic-list-widget
		      :initform nil)
   (category-list-widget :accessor category-list-widget
			 :initform (make-category-list-widget)))
  (:default-initargs :on-dispatch 'forums-dispatcher-handler))

(defmethod dependencies append ((widget forums-dispatcher))
  (list (make-local-dependency :stylesheet "forums")))

(defun configure-topic-widget (w topic-number)
  (let ((topic (find topic-number (get-instances-by-class 'forum-topic)
		     :key #'topic-number))) ; maybe index topic-number slot
    (let ((add-post-widget (third (widget-children w))))
      ;; Bug #140
      (when (typep add-post-widget 'quickform)
        (answer add-post-widget)))
    (setf (topic w) topic)
    (setf (widget-parent w) nil)	    ; dispatcher code will reset this
    (configure-topic-pagination w)))

(defmethod get-widget-for-tokens ((disp forums-dispatcher) uri-tokens)
  (let* ((tokens (remaining-tokens uri-tokens))
	 (tok (first tokens))
	 (w nil))
    (cond ((null tok)
	   ;; default: list all topics by category
	   ;(values (topic-list-widget disp) nil nil))
	   (category-list-widget disp))
	  ((string-equal tok "category")
	   ;; Display a category.  The next token is supposed to be
	   ;; the category number.
	   (let ((n (ignore-errors (parse-integer (first (rest tokens))))))
	     (if (null n)
		 (progn (pop-tokens uri-tokens (length tokens))
			(disp-cached disp))
		 (progn (pop-tokens uri-tokens (length tokens))
			(make-topic-list-widget (get-model 'forum-category n))))))
	  ((string-equal tok "topic")
	   ;; Display a topic.  The next token is supposed to be the
	   ;; topic number.
	   (let ((n (ignore-errors (parse-integer (first (rest tokens))))))
	     (if (null n)
		 (progn (pop-tokens uri-tokens (length tokens))
			(disp-cached disp))
		 (progn (setq w (topic-widget disp))
			(configure-topic-widget w n)
			(pop-tokens uri-tokens (length tokens))
			#+rme
			(format *trace-output* "~&used = ~s, remaining = ~s"
				(reverse used) tokens)
			w))))
	   (t
	    ;; For debugging, just show all the passed in tokens,
	    ;; and claim to have used them all.
	    (format nil "tokens = ~s" tokens)))))



