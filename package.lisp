(in-package :cl-user)

(defpackage #:registry
  (:use :cl :weblocks :stdutils :elephant :cl-ppcre :langutils :f-underscore)
  #+ccl (:import-from :ccl #:quit)
  (:shadow :with-html) 
  (:shadowing-import-from :weblocks #:open-store #:close-store)
  (:shadowing-import-from :ps #:@ #:new)
  (:shadowing-import-from :cl-who #:htm #:str #:esc)
  (:shadowing-import-from :stdutils #:compose #:hash-keys #:safe-subseq);; #:start-logging #:stop-logging)
  (:shadowing-import-from :cl-l10n #:format #:formatter)
  (:documentation
   "A web application based on Weblocks."))


(in-package :registry)

(defmacro registry-proclamations ()
  (declaim
   #+registry-development (optimize (speed 1) (space 1) (debug 3) (safety 3))
   #-registry-development (optimize (speed 3) (debug 1) (safety 1) (space 2))))