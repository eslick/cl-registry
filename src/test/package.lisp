(in-package :cl-user)

(defpackage #:lamsight2-test
  (:use :lamsight2 :cl :fiveam :stdutils)
  #+ccl (:import-from :ccl #:quit)
  (:documentation
   "Test code for LAMsight."))
