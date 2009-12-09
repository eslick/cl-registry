(in-package :registry)

(defun return-tokenized-str (string)
  (multiple-value-bind (successp strlen tokenizedstr chunk)
      (tokenize-string string)
    tokenizedstr))

(defun str-to-id-list (string)
  "Cleans up a string, tokenizes it, and converts it to a list of IDs"
  (let ((clean-list (cl-ppcre:split "\\s" (remove-stopwords (return-tokenized-str string))))
	(id-list))
    (lemmatize
     (dolist (term clean-list id-list)
       (push (id-for-token term) id-list)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Process and add a question ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *question-text* (make-hash-table))
(defvar *question-scores* (make-hash-table))
(defvar *term-scores* (make-hash-table))

(defun querymatch-file-import (filename)
  (with-open-file (stream filename)
    (mapc #'(lambda (entry)
	      (querymatch-import (cdr entry) (car entry)))
	  (read stream))
    nil))

(defun process-question (string &optional (id (hash-table-count *question-text*)))
  (let ((scores (compute-tf (str-to-id-list string))))
    (add-question-text string id)
    (add-question-score scores id)
    (add-term-scores scores id)))

(defun add-question-text (string id)
  (setf (gethash id *question-text*) string))

(defun add-question-score (scores id)
  (setf (gethash id *question-scores*) scores))

(defun add-term-scores (scores id)
  (loop for (term . score) in scores do
       (push (cons id score) 
	     (gethash term *term-scores*))))

(defun count-list (list alist)
  (let ((elt (car list)) (count (assoc (car list) alist)))
    (if elt
	(progn
	  (if count
	      (incf (cdr count))
	      (push (cons elt 1) alist))
	  (count-list (cdr list) alist))
	alist)))

(defun compute-tf (list)
  (let ((counts (count-list list nil)) (length (length list)) (tflist))
    (dolist (pair counts tflist)
      (push (cons (car pair) (/ (cdr pair) length)) tflist))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Process a request and supply matching results ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun process-query (query)
  (multiple-value-bind (text successp)
      (gethash (best-match query) *question-text*)
    text))

(defun best-match (query)
  (caar (make-scorelist (str-to-id-list query))))

(defun nbest-match (query n)
  (let ((matches (make-scorelist (str-to-id-list query))))
    (subseq matches 0 (min (1- (length matches)) n))))

(defun make-scorelist (termlist)
  (let (scorelist)
    (loop for term in termlist when (compute-idf term) do
	 (push (compute-idf term) scorelist))
    (sum-and-sort scorelist)))

(defun sum-and-sort (scorelist)
  (let (summedlist)
    (loop for list in scorelist do
	 (loop for (question . score) in list do
	      (let ((existing-pair (assoc question summedlist)))
		(if existing-pair
		    (progn
		      (remove existing-pair summedlist)
		      (push (cons question (+ score (cdr existing-pair))) summedlist))
		    (push (cons question score) summedlist)))))
    (sort summedlist #'> :key #'cdr)))

(defun compute-idf (term)
  (let ((number-documents (hash-table-count *question-text*)) 
	(scores-for-term (gethash term *term-scores*)) idflist)
    (loop for (question . score) in scores-for-term do
	 (push (cons question (* score (log (/ number-documents (length scores-for-term))))) idflist))
    idflist))