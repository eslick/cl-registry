(in-package :flashex)

;; ===============================================
;;  Chart data access
;; ===============================================

(define-api-handler (fx :mime "text/html") (tokens params auth-p)
  (with-html
    (:p "This is a test handler")))



