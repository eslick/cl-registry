(in-package :registry)

;;;; * rating

(defclass star-rating-presentation (dropdown-member-presentation)
  ()
  (:default-initargs :choices (list "*" "**" "***" "****" "*****")))

(define-lisp-value-getter star-rating-presentation (client-value)
  (length client-value))

(define-lisp-value-setter star-rating-presentation (new-value client-value)
  (ecase new-value
    (0 "")
    (1 "*")
    (2 "**")
    (3 "***")
    (4 "****")
    (5 "*****")))
