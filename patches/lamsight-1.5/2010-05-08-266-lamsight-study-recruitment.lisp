;; -*- Mode:lisp; tab-width:2; indent-tabs-mode:nil; -*-

(in-package :registry)

(defpatch (lamsight 1.5 266 study-recruitment) (&key (author (current-user t)))
  (if (stringp author) (setq author (get-user author)))
  (let ((entry
	 (make-instance 'blog-entry
			:title #!"Pulmonary Function and Quality of Life Study"
			:author author
			:content
#!"The following new study is looking for participants:
<P><B>Pulmonary Function and Quality of Life:</B>
Because there is currently no effective treatment or cure for LAM, maintaining quality of life is
especially important. We are interested in studying how the severity of LAM affects the quality of life 
of the worldwide LAM community. 
This information will form an important baseline for future LAM treatment trials. 
In addition, this study aims to determine the accuracy of patient entry of pulmonary function test results
into web-based collection forms in order to potentially open up new avenues of future LAM research
that rely more heavily upon self-report.")))
    ;; Returns
    entry))
