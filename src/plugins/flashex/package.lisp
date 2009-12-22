(in-package :cl-user)

(defpackage #:flashex (:use :cl))

(defun flashex::import-all-registry (&optional (package :flashex))
  (do-symbols (symbol :registry)
    (import symbol package)))

(defun flashex::import-all-smart (&optional (package :flashex))
  (do-symbols (symbol :smart)
    (import symbol package)))

(eval-when (:load-toplevel)
  (flashex::import-all-registry))


