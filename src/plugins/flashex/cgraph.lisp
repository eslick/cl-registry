(in-package :flashex)

;; ===============================================
;;  Graphical causal models
;; ===============================================

(define-api-handler (fx-graph :mime "text/html") (tokens params auth-p)
  (with-html
    (json:encode-json-to-string 
     '#(((name . node1) (links node2))
	((name . node2) (links node4))
	((name . node3) (links node4))
	((name . node4) (links))))))


