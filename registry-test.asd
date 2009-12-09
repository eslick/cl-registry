;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
(defpackage #:registry-test-asd
  (:use :cl :asdf))

(in-package :registry-test-asd)

(defsystem registry-test
    :name "registry-test"
    :version "0.1"
    :maintainer ""
    :author ""
    :licence ""
    :description "registry testing"
    :components ((:module src
		  :components ((:module test
			        :components ((:file "package")
                                             (:file "tests")
                                             (:file "models")
                                             )
				:serial t))
		  :serial t
		  :depends-on ()))
    :serial t
    :depends-on (:registry :fiveam :selenium :closer-mop :stdutils))


