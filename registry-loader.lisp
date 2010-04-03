; -*- mode: lisp -*-

(cl:defpackage #:registry-loader
    (:use #:cl :ccl)
  (:export #:add-to-registry
           #:add-systems-to-registry
           #:loadsys
           #:load-swank
           #:start-registry
           #:*source-directory*))

(in-package #:registry-loader)

(defvar *source-directory*
  (make-pathname :name nil :type nil
                 :defaults (or *load-pathname* *default-pathname-defaults*))
  "The directory that holds the source files, which is assumed
   to be the same directory that this file is being loaded from.")

(require "asdf")

(defun add-to-registry (&rest paths)
  (dolist (path paths)
    (pushnew (truename (merge-pathnames path *source-directory*))
             asdf:*central-registry*
             :test #'equal)))

(defun add-systems-to-registry ()
  (let ((systems-wildcard
         (merge-pathnames
          (make-pathname :directory "../systems" :name :wild :type :wild)
          *source-directory*)))
    (apply 'add-to-registry
           (directory systems-wildcard :directories t :files nil))))

(defun loadsys (system)
  (asdf:oos 'asdf:load-op system))

(progn
  (add-systems-to-registry)
  ;; Oh, joy! Darcs doesn't support symbolic links.
  (add-to-registry "../systems/weblocks/src/store/elephant")
  (add-to-registry "../systems/weblocks/src/store/memory")
  (add-to-registry "../systems/weblocks/src/store/prevalence")
  (add-to-registry "."))

(defun load-swank (&optional port)
  (when (and port (integerp port))
    (loadsys :swank)
    (let ((sym (find-symbol "*DEFAULT-WORKER-THREAD-BINDINGS*" :swank)))
      (when (and sym (boundp sym) (listp (symbol-value sym)))
        (set sym (cons (cons '*package* (or (find-package :registry)
                                            (find-package :registry-loader)))
                       (symbol-value sym)))))
    (funcall (find-symbol "CREATE-SERVER" :swank)
             :port port
             ;;:coding-system "utf-8-unix"
             :dont-close t)))

(defparameter *default-config* '("ilr-production" "devel"))

(defun start-registry (port &optional config)
  (when (stringp port)
    (setq port (ignore-errors (parse-integer port))))
  (cond ((null config)
         (setf config *default-config*))
        ((stringp config)
         (setf config (funcall (find-symbol "SPLIT-SEQUENCE" :split-sequence)
                               #\+
                               config))))
  (check-type config list)
  (when port
    (let ((start-registry (find-symbol "START-REGISTRY" :registry)))
      (when (fboundp start-registry)
        (funcall (fdefinition start-registry)
                 :address (or (ccl:getenv "REGADDR")
			      "localhost")
                 :port port
                 :config config)))))

(defun is-environment-p (var)
  (member (ccl:getenv var) '("YES" "yes" "true" "TRUE")
              :test #'equal))

(when (is-environment-p "REGDEV")
  ;; Enables debugging optimizations, and a few other things, if set at compile time
  (pushnew :registry-development *features*))

(when (is-environment-p "REGISTRY_PRODUCTION")
  (pushnew :registry-production *features*))

(unless (is-environment-p "NOLOAD")
  (loadsys :trivial-backtrace)
  (loadsys :registry)
  (start-registry (ccl:getenv "REGPORT") (ccl:getenv "REGCONFIG")))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Copyright 2009 Bill St. Clair
;;;
;;; Licensed under the Apache License, Version 2.0 (the "License");
;;; you may not use this file except in compliance with the License.
;;; You may obtain a copy of the License at
;;;
;;;     http://www.apache.org/licenses/LICENSE-2.0
;;;
;;; Unless required by applicable law or agreed to in writing, software
;;; distributed under the License is distributed on an "AS IS" BASIS,
;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;; See the License for the specific language governing permissions
;;; and limitations under the License.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
