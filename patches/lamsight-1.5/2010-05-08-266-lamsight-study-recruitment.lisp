;; -*- Mode:lisp; tab-width:2; indent-tabs-mode:nil; -*-

(in-package :registry)

(defpatch (lamsight 1.5 266 study-recruitment) (mode filename)
  (ecase mode
    (:export
     (let ((counter 0.)
           (blog-entries
            (select-if
             (lambda (obj)
               (string= (blog-entry-title obj)
                        "Pulmonary Function and Quality of Life Study"))
             (get-instances-by-class 'blog-entry)))
           (articles
            (loop for title in '("lam-qol-study-consent-form"
                                 "lam-qol-study-ilr-data-use-form"
                                 "lam-qol-study-articles")
               append (articles-for-pagename title))))
       (with-open-file (out filename :direction :output :if-exists :supersede :if-does-not-exist :create)
         (flet ((export-some-instances (objs)
                  (let ((*package* (find-package :registry)))
                    (format out "~s~%" `(model ,(class-name (class-of (first objs)))))
                    (dolist (obj objs)
                      (format out "~s~%" (export-instance obj))
                      (incf counter)))))
           (export-some-instances blog-entries)
           (export-some-instances articles)
           ;; Returns
           counter))))
    (:import
     (import-model-file filename))))
