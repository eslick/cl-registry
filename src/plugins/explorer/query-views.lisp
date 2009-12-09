(in-package :registry)

(registry-proclamations)

(defwidget browse-queries () ()) ;; find things to look at

;; View and modify queries:
;; - See text, visualization of answer, data, etc. 
;; - Browse and choose populations (patients that use oxygen and live in Europe)
(defwidget query-editor ()
  ())

;; Population builder:
;; - Choose questions & values (conjunctions)
;; - Merge populations (intersection)
;; - Combine populations (union)
(defwidget population-editor ()
  ())


;; DIALOGS

;; Population graph view (graphviz callout?)
;; Help dialog
;; Comment dialog retasking
