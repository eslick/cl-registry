










#|


;;
;; First pass implementation of the LAMsight explorer
;;


;;
;; Search interface to pull up questions
;;
;; basic per-question stats
;; - how many responses vs. users
;; - distribution of responses
;;
;; Survey browser
;;


(defun make-explorer-page ()
  (make-instance 'composite :dom-id "explorer" :widgets
		 (list (make-search-bar "Search survey questions" 'do-explorer-search 'search)
		       (make-default-explorer-content 'content)
		       (make-instance 'explorer-search-results :results nil :peer-name 'results))))
;;		       (make-top-searches-widget 'top-searches)
;;		       (make-top-hypotheses-widget 'top-hypotheses))))

(defun do-explorer-search (peer-widget query)
  "Lookup a list of objects and go to that view"
  (let ((results (select-if (lambda (obj) (eq (type-of obj) 'question))
			    (fulltext-search query))))
    (update-results peer-widget results (if results nil :no-results))))
		  

(defun update-results (peer-widget results state)
  (let ((content-widget (get-peer-widget peer-widget 'content))
	(results-widget (get-peer-widget peer-widget 'results)))
    (setf (content-state content-widget) state)
    (setf (search-results results-widget) results)))

;;
;; Stubs
;;

(defwidget explorer-content (peer-mixin widget)
  ((state :initarg :state :accessor content-state :initform :intro)))

(defmethod render-widget-body ((widget explorer-content) &rest args)
  (declare (ignore args))
  (case (content-state widget)
    (:intro
     (with-html
       (:p "This is a very preliminary interface to explore survey content")
       (:p "Try typing keywords from the surveys here such as 'exercise'")))
    (:no-results
     (with-html
       (:h2 "No search results were found!")))))

(defun make-default-explorer-content (name)
  (make-instance 'explorer-content :peer-name name))

(defun make-top-searches-widget (name)
  (declare (ignore name))
  (lambda ()
    (with-html
      (:div :class "top-searches"
	    "Top searches:"))))

(defun make-top-hypotheses-widget (name)
  (declare (ignore name))
  (lambda ()
    (with-html
      (:div :class "top-hypotheses"
	    "Top hypotheses:"))))
  

;; ========================================
;;  Explorer search display
;; ========================================




;;
;; Rendering support for list view
;;

(defmethod render-widget-body ((widget explorer-search-results) &rest args)
  (declare (ignore args))
  (when (search-results widget)
    (with-html 
      (render-link (lambda (&rest args)
		     (declare (ignore args))
		     (update-results widget nil :intro))
		   #!"Clear Answers")
      (loop for question in (search-results widget) do
	   (htm (:div :class "explorer-question-result-view"
		      ;;		    (:span :class "x1" (:span :class "x1a")) 
		      ;;		    (:span :class "x2" (:span :class "x2a"))
		      (:div :class "mod-content"
			    (:div :class "bd"
				  (:div :class "explorer-question-summary"
					(render-question-details question)
					(render-user-question-response question))
				  (:div :class "explorer-chart-summary" 
					(let ((chart (make-question-chart question '(("chs" "200x140")))))
					  (if chart
					      (htm (:img :src chart))
					      (htm (:p "No Chart View <br> Available")))))
				  (:div :class "explorer-question-bottom")))))))))


;; <div id="mod-id" class="mod">
;; <span class="x1"><span class="x1a"></span></span>
;; <span class="x2"><span class="x2a"></span></span>
;;    <div class="mod-content">
;;      <div class="hd">
;;         <h3>Module head</h3>
;;      </div>
;;      <div class="bd">
;;        <p>Module body</p>
;;      </div>
;;      <div class="ft">
;;          <p class="ft-wrapper">Module foot</p>
;;      </div>
;;    </div>
;; </div>

(defun render-question-details (question)
  (with-html
    (:h1 (str (question-prompt question)))
    (:h2 "Possible answers include: " 
	 (render-possible-answers question))))

(defun render-user-question-response (question)
  (let* ((answers (get-answers question))
	 (valid-answers (filter-if #'null answers :key #'value))
	 (answer (find (current-user) answers :key #'user)))
    (with-html
      (:p "Your answer: "
	  (if answer
	      (htm (:b (str (format nil "~A" (case (value answer)
					       (:true "Yes") (:false "No"))))))
	      (str "You have not answered this question"))
	  (htm (:br))
	  "Survey takers: " (str (format nil "~A" (length answers)))
	  "| Valid responses: " (str (format nil "~A" (length valid-answers)))))))
      

(defun render-possible-answers (question)
  (with-html
    (dolist (option (possible-answers question))
      (str (cl-user::format nil "~A &nbsp;" option)))))

;; 
;; Compute chart
;;

(defun make-question-chart (question &optional (parameters *chart-defaults*))
  (let ((chart-type (derive-chart-type question)))
    (when chart-type
      (make-chart chart-type
		  parameters
		  nil
		  (make-dataset
		   :type chart-type
		   :labels (question-labels chart-type question)
		   :values (filter-answers chart-type question))))))

(defmethod question-labels ((ctype (eql :pie)) question)
  (case (question-data-type question)
    (:boolean '("yes" "no" "no-answer"))
    (t (error "Unsupported data type: ~A" (question-data-type question)))))

(defun possible-answers (question)
  (ignore-errors
    (case (question-data-type question)
      (:boolean (list "Yes" "No"))
      (:string (case (question-view-type question)
		 (:choice (question-choices question))
		 (:multichoice (question-choices question)))))))

(defun derive-chart-type (question)
  (case (question-data-type question)
    (:boolean :pie)
    (:string
     (case (question-view-type question)
       ((or :choice :multichoice)
	:pie)
       (:auto
	nil)))
    (t nil)))
;;    (:number :bar)))

(defmethod filter-answers ((ctype (eql :pie)) question)
  (assert (eq (question-data-type question) :boolean))
  (let ((answers (get-answers question))
	(yes 0) (no 0))
    (loop for answer in answers do
	 (case (value answer)
	   (:true (incf yes))
	   (:false (incf no))))
    (list yes no (- (get-user-count) (length answers)))))

(defun get-user-count ()
  (count-persistent-objects *default-store* 'user))


|#