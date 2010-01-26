(in-package :flashex)

;; Get graph node expansions for the Hypothesis Space explorer

(defparameter *test-graph*
  '(((short . node1) (full . "Node1 Is Big") (type . question) (links node2)) 
    ((short . node2) (full . "Node2 Is Big") (type . symptom) (links node3 node4))
    ((short . node3) (full . "Node3 Is Big") (type . intervention) (links))
    ((short . node4) (full . "Node4 Is Big") (type . intervention) (links))))


(define-api-handler (fx-graph :mime "text/html") (tokens params auth-p)
  (let ((query (assoc-get "query" params #'equal))
	(test (assoc-get "test" params #'equal)))
    (when query
      (if test
	  (with-html
	    (json:encode-json-to-string *test-graph*))
	  ;;	(when auth-p
	  (with-html
	    (json:encode-json-to-string 
	     (make-query-graph query)))))))

;;
;; Hypothesis and Symptom DB
;;

(defparameter *symptoms* nil)
(defparameter *interventions* nil)
(defparameter *symptom-query-map* nil)
(defparameter *intervention-query-map* nil)

(defun get-symptoms ()
  (aif-ret *symptoms*
    (setf *symptoms*
	  (smart::with-lam-listserv ()
	    (with-transaction ()
	      (append
	       (mapcar #'smart::text-annotation->string
		       (append (smart::all-text-annotations-by-type 'smart::symptom)
			       (smart::all-text-annotations-by-type 'smart::outcome)))
	       (formal-symptoms-dataset)))))))

(defun formal-symptoms-dataset ()
  (smart::with-lam-listserv ()
    (map-class 
     (lambda (dataset)
       (when (equal (smart::dataset-title dataset)
		    "SYMP Ontology single-term matches in listserv corpus")
	 (return-from formal-symptoms-dataset (smart::dataset-data dataset))))
     'smart::dataset)))

(defun get-interventions ()
  (aif-ret *interventions*
    (setf *interventions*
	  (smart::with-lam-listserv ()
	    (with-transaction ()
	      (mapcar #'smart::text-annotation->string
		      (smart::all-text-annotations-by-type 'smart::intervention)))))))


(defun get-symptom-query-map ()
  (aif-ret *symptom-query-map*
    (setf *symptom-query-map*
	  (make-term-query-map (make-hash-table :test #'equal)
			       (get-symptoms)))))

(defun get-intervention-query-map ()
  (aif-ret *intervention-query-map*
    (setf *intervention-query-map*
	  (make-term-query-map (make-hash-table :test #'equal)
			       (get-interventions)))))

(defun make-term-query-map (hash data)
  (loop for statement in data do
       (loop for word in (extract-words statement) do
	    (unless (gethash word hash)
	      (setf (gethash word hash) nil))
	    (push statement (gethash word hash))))
  hash)

(defun term-query (terms &optional type)
  (loop for term in terms
       nconc (copy-list (gethash term (if (eq type :intervention)
					  (get-intervention-query-map)
					  (get-symptom-query-map))))))
	   

(defun terms->query-graph-nodes (terms type)
  (loop for term in terms collect
       (make-graph-node term term nil type nil)))

;;
;; Query graph components-graph-search (query) \\
;;



(defun make-query-graph (query)
  (let* ((qtree (make-question-subgraph query)))
;;	 (nodes (remove-duplicates 
;;	 (get-term-nodes query)
;;		 :test #'equal :key (f_ (assoc-get :id _)))))
    (setf (cdr (assoc :links (first qtree)))
	  (mapcar (f_ (assoc-get :id _)) (rest qtree)))
    (cons (make-graph-node query query 0 "query"
			   (mapcar (f_ (assoc-get :id _)) 
				   qtree)) ;;(first qtree)))
	  qtree)))
;;				   (cons (first qtree) nodes)))
;;	  (append qtree nodes))))


(defun make-question-subgraph (query)
  (remove-duplicates 
   (mapcar #'question->graph-node (query-graph-search query))
   :test #'equal :key #'cdar))


(defun get-term-nodes (query)
  (append (terms->query-graph-nodes 
	   (remove-duplicates
	    (term-query (extract-words query) :intervention)
	    :test #'equal)
	   :intervention)
	  (terms->query-graph-nodes 
	   (remove-duplicates
	    (term-query (extract-words query) :symptom)
	    :test #'equal)
	   :symptom)))


(defun query-graph-search (query)
  (let ((questions (contentful-questions
		    (fulltext-search-for-types 
		     (or query "lam") 
		     '(question)))))
    (when questions
      (subseq questions 0 (min 10 (length questions))))))

(defun contentful-questions (questions)
  (with-transaction ()
    (select-if (lambda (q) 
		 (published-object-p q))
;;		      (get-instance-by-value 'answer 'question q))
	       questions)))

(defun make-graph-node (short long id type links)
  `((:short . ,short)
    (:full . ,long)
    (:id . ,id)
    (:type . ,type)
    (:links . ,links)))

(defun question->graph-node (question &optional links)
  (assert (eq (type-of question) 'question))
  (let* ((name (question-prompt question))
	 (trunc (remove-stopwords name)))
    `((:short . ,(subseq trunc 0 (min (length trunc) 30)))
      (:full . ,name)
      (:id . ,(mid question))
      (:type . question)
      (:links . ,nil))))


(defun perform-g-test (q1 q2)
  (ignore-errors
    (let ((paired-samples 
	 (loop for person in (mapcar #'user (get-answers q1))
	    for a1 = (first (get-user-answers q1 person))
	    for a2 = (first (get-user-answers q2 person))
	    when (and a1 a2)
	    collect (cons (value a1) (value a2)))))
    (append (multiple-value-list 
	     (cl-mathstats::g-test
	      (cl-mathstats::make-contingency-table 
	       (cars paired-samples) (cdrs paired-samples))))
	    (list q1 q2)))))
	  

(defun question-g-tests (q1 questions)
  (sort (remove-nulls (mapcar (curry 'perform-g-test q1) questions)) 
	#'< :key #'second))

(defvar *all-g-tests* nil)

(defun all-g-tests (questions)
  (loop for question in questions do
       (push (list question (question-g-tests question questions))
	     *all-g-tests*)))

(defvar *significance-test1* nil)
(defvar *significance-test2* nil)

(define-api-handler (fx-assoc :mime "text/html")
    (tokens params auth-p)
  (json:encode-json-to-string 
   (let ((query (assoc-get "query" params #'equal)))
     (cond ((and (equal query "test1") *significance-test1*)
	    (make-question-assoc-graph *significance-test1*))
	   ((and (equal query "test1") *significance-test2*)
	    (make-question-assoc-graph *significance-test2*))))))

(defun make-question-assoc-graph (assoc-recs)
  (let ((hash (make-hash-table)))
    (loop for (gscore gsig df q1 q2) in assoc-recs collect
	 (let* ((qn1 (get-qgraph-node q1 hash))
		(qn2 (get-qgraph-node q2 hash))
		(firstp (if (< (mid q1) (mid q2)) t nil))
		(head (if firstp qn1 qn2))
		(tail (if firstp qn2 qn1))
		(id   (assoc-get :id tail))
		(links (assoc :links head)))
	   (unless (member id (cdr links))
	     (push id (cdr links)))))
    (hash-values hash)))

(defun get-qgraph-node (question hash)
  (aif-ret (gethash (mid question) hash)
    (setf (gethash (mid question) hash)
	  (question->graph-node question))))

