(in-package :registry)

(registry-proclamations)

;;
;; Reporting plugin definition
;;

(define-plugin report (site-app)
  "The create function accepts a list of plugins"
  :tab-name 'report
  :create 'make-report-page)

(defun make-report-page (&rest args)
  (declare (ignore args))
  (let* ((w (make-instance 'composite)))
    (setf (composite-widgets w) (list (make-instance 'simple-report)))
    w))

