(in-package :registry)

(registry-proclamations)

(defun upgrade-lamsight-2009-to-2011-11-01 ()
  (map-class (lambda (q)
	       (setf (question-hipaa-id-p q) nil)
	       (setf (question-number q) nil))
	     'question
	     :collect nil))

	     