(in-package :registry)

(registry-proclamations)

;; ==============================================================
;;  Diary support
;; ==============================================================

(defmodel diary-answer (answer)
  ((ref-value :accessor reference-value :initarg :reference-value)
   (ref-duration :accessor reference-duration :initarg :reference-duration)))

(defun make-duplicate-answer (question value reference &optional duration)
  (declare (ignore value))
  (make-instance 'diary-answer 
		 :question question
		 :user (current-user)
		 :entry-time (get-universal-time)
		 :reference-value reference
		 :reference-duration duration))

