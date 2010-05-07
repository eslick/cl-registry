(in-package :cl-user)

(defpackage #:registry-test
  (:use :registry :cl :fiveam :stdutils)
  #+ccl (:import-from :ccl #:quit)
  (:documentation
   "Test code for Registry"))
