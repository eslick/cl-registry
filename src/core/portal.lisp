(in-package :registry)

(registry-proclamations)

;; Known portals (application names) derived from site names in configuration files:
;;   LAMsight
;;   ILR == International LAM Registry
;;

(defconstant *registry-known-portals '(:lamsight :ilr))

(defvar *registry-default-portal* ':lamsight)

(defun get-portal-name (&key
			(site-name (get-site-config-param :site-name))
			(default *registry-default-portal*))
  (cond
    ((null site-name) default)
    ((string-equal site-name "International LAM Registry") ':ilr)
    ((string-equal site-name "LAMsight") ':lamsight)
    (t default)))
