(in-package :flashex)

;; Get WordTree from listserv

;;(define-api-handler (wordtree :mime "application/json")
;    (tokens params auth-p)
;  (destructuring-bind (&key :prefix-string string &allow-other-keys) params
;    (with-html 
;      (



;;
;; Annotation distribution
;;

(define-api-handler (fx-treemap :mime "text/plain")
    (tokens params auth-p)
  (with-html
    (json:encode-json-to-string (get-cached-treemap))))

(defparameter *cached-treemap* nil)

(defun get-cached-treemap ()
  (with-store (smart::*listserv-db-spec*)
    (aif-ret *cached-treemap*
      (setf *cached-treemap* (smart::make-msg-treemap)))))

     