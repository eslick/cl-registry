(in-package :registry)

(registry-proclamations)

;; Placeholder...

(defmodel hypothesis ()
  ())


;; ==================================================================
;;  Represents a question being asked of the database
;; ==================================================================

(defmodel query ()
  ((query :accessor query-text :initarg :query
	 :documentation "The text of the question")
   (type :accessor query-type :initarg :type :initform :distribution
	 :documentation "What kind of question is this; distribution, regression, etc.")
   (populations :accessor query-populations :initarg :populations :initform nil
		:documentation "Populations necessary to answer the questions")
   (views :accessor query-views :initarg :views :initform nil
	  :documentation "Views of the data")))

(defmethod humanize-name ((inst query))
  (humanize-name (query-text inst)))

(defmethod attributize-name ((inst query))
  (attributize-name (subseq (query-text inst) 0 20)))


;; For browsing

(defmethod results-title ((inst query))
  (query-text inst))



