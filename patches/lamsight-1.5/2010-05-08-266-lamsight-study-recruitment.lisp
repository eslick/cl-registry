;; -*- Mode:lisp; tab-width:2; indent-tabs-mode:nil; -*-

(in-package :registry)

(defpatch (lamsight 1.5 266 study-recruitment) (mode &key owner filename)
  "Usage: (RUNPATCH (LAMSIGHT 1.5 266 STUDY-RECRUITMENT) MODE ARGS...)
Mode is one of: :DELETE, :EXPORT, :IMPORT
 :DELETE - drop instances, blog entry and articles for LAMsight study recruitment
 :EXPORT :FILENAME <path> - export instances to file
 :IMPORT :FILENAME <path> :OWNER <user> - import instances from file and assign to <user>
"
  (flet ((find-blog-entries ()
           (select-if
            (lambda (obj)
              (string= (blog-entry-title obj)
                       "Pulmonary Function and Quality of Life Study"))
            (get-instances-by-class 'blog-entry)))
         (find-articles ()
           (loop for title in '("lam-qol-study-consent-form"
                                "lam-qol-study-ilr-data-use-form"
                                "lam-qol-study-articles")
              append (articles-for-pagename title))))
    (ecase mode
      (:delete
       (mapcar #'drop-instance (find-blog-entries))
       (mapcar #'drop-instance (find-articles)))
      (:export
       (let ((counter 0.)
             (blog-entries (find-blog-entries))
             (articles (find-articles)))
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
       (import-model-file filename)
       ;; Ownership may have been lost on import
       (when owner
         (mapcar #'(lambda (obj) (setf (blog-entry-author obj) owner)) (find-blog-entries))
         (mapcar #'(lambda (obj) (setf (article-owner obj) owner)) (find-articles)))))))
