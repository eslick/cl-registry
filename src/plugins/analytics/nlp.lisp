(in-package :registry)

(registry-proclamations)

;; ==================================================
;;  Cheap NLP
;; ==================================================

(defvar *stopwords* (make-hash-table :test #'equal))

(define-system-event-hook load-stopwords (start-app)
  "Initialize the stopword tables when the application starts up"
  (with-open-file (stream
		   (make-pathname :name "stopwords" :type "txt"
				  :defaults (registry-relative-path '("data")))
		   :external-format :ascii)
    (do-contentful-lines (line stream)
      (hash-put *stopwords* (string-trim " " line) t)))
  (with-open-file (stream
		   (make-pathname :name "extra-stopwords" :type "txt"
				  :defaults (registry-relative-path '("data")))
		   :external-format :ascii)
    (do-contentful-lines (line stream)
      (hash-put *stopwords* (string-trim " " line) t))))

(defun stopword-p (word)
  (gethash word *stopwords*))

(defun remove-stopwords (string &optional (downcase t))
  (unsplit-words 
   (filter-if #'stopword-p 
	      (extract-words 
	       (if downcase 
		   (string-downcase string) 
		   string)))))

;;
;; Word cloud computation
;;

(defun filter-empty-strings (list)
  (filter-if (lambda (string) (equal string "")) list))

(defparameter *strip-punctuation*
  '(#\, #\- #\/ #\. #\( #\) #\> #\< #\:))


(defun strip-punctuation (list)
  (mapcar #'(lambda (string)
	      (string-remove-characters 
	       string *strip-punctuation*))
	  list))

(defparameter *max-wordcloud-string-length* 250)
 

(defun make-word-cloud-dataset (strings)
  `((("string" "text")
     ,@(filter-empty-strings
	(mapcar #'remove-stopwords
		(strip-punctuation 
		 (filter-if (lambda (str)
			      (> (length str) *max-wordcloud-string-length*))
			    strings)))))))

(defun select-top-n-trings (strings n)
  "Filter strings but keep original distribution"
  (let ((selected (top-n-strings strings n)))
    (filter-if (lambda (string)
		 (member string selected :test #'equalp))
	       strings)))

(defun top-n-strings (strings n)
  "Returns the top n occuring strings from a list of strings"
  (cars
   (safe-subseq 
    (sort (count-strings strings) #'> :key #'cdr)
    0 n)))

(defun count-strings (strings)
  "Good for short strings; n^2 complexity"
  (let ((alist nil))
    (loop for string in strings do
	 (aif (assoc string alist :test #'equalp)
	      (incf (cdr it))
	      (push (cons string 1) alist)))
    alist))