(in-package :registry)

(registry-proclamations)

(defmodel research-query ()
  ((query-text :accessor query-text :initarg :text)
   ;; What kind of query is this; population, association, variation, error, etc?
   (query-type :accessor query-type :initarg :type)
   ;; the populations participating in this query
   (populations :accessor populations :initarg :populations :initform nil)
   ;; what kinds of views we have
   (result-views :accessor query-result-views :initarg :views :initform nil)
   ;; filters; what sorts of parameters do we want to filter on?
   (result-filters :accessor query-result-filters :initarg :filters :initform nil)
   ;; users who have edited this query
   (editors :accessor query-editors :initarg :editors :initform nil)))


;; Comments on query

(defmodel research-population ()
  ((name :accessor population-name :initarg :name)
   ;; The set of questions & parameters used to generate this population
   (query-set :accessor population-query-set :initarg :query-set)))
   

