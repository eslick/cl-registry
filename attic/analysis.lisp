(in-package :registry)

;;
;; Analytical tools
;;

(defun get-element-counts (element-list &optional (hash (make-hash-table)))
  (dolist (elt element-list)
    (unless (gethash elt hash)
      (setf (gethash elt hash) 0))
    (incf (gethash elt hash))))

;;
;; Specific questions
;;
#|
(defun question-for-id (id &optional (questions questions))
  (find id questions :key #'answer-question

(defun get-answer-distribution (&optional (answers answers))
  (get-element-counts |#