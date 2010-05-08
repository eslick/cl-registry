;; -*- Mode:Lisp; tab-width:2; indent-tabs-mode:nil; -*-

(in-package :registry)

(defun patch-function-name (app version ticket tag)
  "Construct a canonical function name for patch referred to by APP, VERSION, TICKET, and TAG."
  (intern (format nil "PATCH-~A~@[-~A~]~@[-~A~]~@[-~A~]"
		  (or app "REGISTRY") version ticket (and tag (string tag)))))

(defmacro defpatch ((app version ticket tag) args &body body)
  "Define patch function for APP, VERSION, TICKET, and TAG.
Patch function will be defined as (LAMBDA ARGS . BODY)"
  `(defun ,(patch-function-name app version ticket tag) ,args
     ,@body))

(defmacro runpatch ((app version ticket tag) &rest args)
  `(,(patch-function-name app version ticket tag) ,@args))
