(in-package :registry)

(registry-proclamations)

;;
;; Support flagging data objects as [non]published == visible for analytics and reporting
;;

;; TODO: implement checking for policy and context, for example:
;;   do not publish HIPAA identifiers (policy) unless in privileged mode (context)

(defpclass published-data-mixin ()
  ())

(defgeneric published-data-p (instance)
  (:documentation "A generic function to be implemented by inheriting classes.
Returns non-NIL if instance is published (i.e. visible) for purposes of analytics and reporting.")
  (:method ((instance t)) nil))
