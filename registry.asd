;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;; Copyright (c) 2008-2010, Massachusetts Institute of;Technology. All rights reserved. 
;;; Copyright (c) 2008-2010, LAM Treatment Alliance. All rights reserved. 
;;; Released under a BSD-style license: http://www.opensource.org/licenses/bsd-license.php 
;;; See LICENSE file 

(defpackage #:registry-asd
  (:use :cl :asdf))

(in-package :registry-asd)

;; Required by cl-l10n
#+ccl (setq ccl:*default-file-character-encoding* :utf-8)

(defsystem registry
    :name "Medical Registry"
    :version "0.9"
    :maintainer "Clozure Associates"
    :author "MIT Media Lab / Clozure Associates"
    :licence "BSD"
    :description "A general medical registry platform for patients and clinicians"
    :components ((:file "package")
		 (:module conf
		  :components ((:file "conf") ;; site config
			       (:file "vars") ;; global vars
			       (:file "stores") ;; weblocks store
			       (:file "hooks")  ;; system hooks
			       (:file "logging")) ;; debug/logging
		  :depends-on ("package")
		  :serial t)
		 (:module src
		  :components 
		  ((:file "registry")
		   (:module libs
			    :components 
			    ((:file "datetime")
			     (:file "utils" :depends-on ("datetime"))
			     (:file "model" :depends-on ("utils"))
			     ;; Text content management
			     (:module fulltext
				      :components
				      ((:file "tokenization")
				       (:file "fulltext")
				       (:file "fulltext-mixin"))
				      :depends-on ("model")
				      :serial t)
			     (:module internationalization
				      :components 
				      ((:file "google-translation")
				       (:file "translation-mixin")
				       (:file "translation")
				       (:file "i18n"))
				      :depends-on ("utils" "model")
				      :serial t)
			     ;; Presenting the data model
			     (:module presentations
				      :components 
				      ((:file "boolean" :depends-on ("presentation-protocol"))
				       (:file "choice" :depends-on ("presentation-protocol"))
				       (:file "date-time" :depends-on ("presentation-protocol" "string"))
				       (:file "measurement" :depends-on ("presentation-protocol" "number"))
				       (:file "number" :depends-on ("presentation-protocol"))
				       (:file "presentation-protocol")
				       (:file "star-rating" :depends-on ("presentation-protocol"))
				       (:file "string" :depends-on ("presentation-protocol"))
				       (:file "tinymce")
				       (:file "wmd"))
				      :depends-on ("utils" internationalization))
			     (:module views
				      :components
				      ((:file "post-view")
				       (:file "types"))
				      :depends-on ("utils" presentations)
				      :serial t)
			     (:module published-data
				      :components
				      ((:file "published-data-mixin")))
			     ;; Visualization
			     (:module visualization
				      :components
				      ((:file "cache")
				       (:file "visualizations"))
				      :depends-on ("utils")
				      :serial t)
			     (:module geocoding
				      :components
				      ((:file "geo"))
				      :depends-on ("utils"))
			     ;; UI Libraries
			     (:module workspaces
				      :components
				      ((:file "dispatcher")
				       (:file "dynamic-navigation")
				       (:file "workspace")
				       (:file "rounded-composite")
				       (:file "flash"))
				      :depends-on ("utils")
				      :serial t)
			     )
			    :depends-on ("registry"))
		   (:module core
		    :components 
		    ((:file "events")
		     (:file "data-api")
		     (:file "plugins")
		     (:file "permissions")
		     (:module models
			      :components
			      ((:file "address")
			       (:file "user")
			       (:file "patient")
			       (:file "preferences")
			       (:file "article")
			       (:file "registration"))
			      :serial t)
		     (:module widgets
			      :components
			      ((:file "login-widget")
			       (:file "register")
			       (:file "change-password")
			       (:file "email-unsubscribe")
			       (:file "preferences-dialog")
			       (:file "articles")
			       (:file "quick-help")
			       (:file "content"))
			      :serial t)
		     (:file "authentication")
		     (:file "handlers")
		     (:file "portal")
		     (:file "page-template")
		     (:file "front-page")
		     (:file "registry-nav")
		     )
                    :depends-on (libs)
		    :serial t)
		   (:module data-model
		    :components
		    ((:file "question")
		     (:file "answer")
		     (:file "group")
		     (:file "group-rules")
		     (:file "survey")
		     (:file "study")
		     (:file "survey-rules")
                     (:file "provenance"))
		    :depends-on (core)
		    :serial t)
 		   (:module plugins
		    :components
		    ((:file "twitter")
		     (:file "featured-survey")
		     (:file "import-export")
		     (:file "hlog")
		     (:file "user-map-widget")
		     (:module comments
			      :components
			      ((:file "comment")
			       (:file "comment-dialog"))
			      :serial t)
		     (:module blog
			      :components
			      ((:file "blog-entry")
			       (:file "blog"))
			      :serial t)
		     (:module email
			      :components
			      ((:file "smtp")
			       (:file "email")
			       (:file "digest")
			       (:file "message-template")
			       (:file "announcement"))
			      :depends-on (forums)
 			      :serial t)
		     (:module analytics
			      :components
			      ((:file "statistics")
			       (:file "nlp")
			       (:file "queries"))
			      :serial t)
		     (:module qualitymetric
			      :components
			      ((:file "sf36")
			       (:file "qualitymetric")))
		     (:module ilr-surveys
			      :components
			      ((:file "ilr-surveys")
			       (:file "sgrq")
			       (:file "sf36")
			       (:file "lam-history")
			       (:file "ilr-beta"))
			      :depends-on (analytics qualitymetric)
			      :serial t)
		     (:module admin
			      :components
			      ((:file "admin")
			       (:file "admin-page"))
			      :depends-on (email)
			      :serial t)
		     (:module forums
			      :components
			      ((:file "ratings")
			       (:file "forum")
			       (:file "recent-topics")
			       (:file "recent-activity")
			       (:file "forums"))
			      :serial t)
		     (:module studies
			      :components
			      ((:file "lamsight-estrogen")))
		     (:module collect
			      :components
			      ((:file "survey-state")
			       (:file "study-ctrl")
			       (:file "survey-ctrl"))
			      :depends-on (comments)
			      :serial t)
		     (:module create
			      :components
			      ((:file "survey-widgets")
			       (:file "survey-editor"))
			      :depends-on (collect)
			      :serial t)
		     (:module explorer
			      :components
			      ((:file "population")
			       (:file "query")
			       (:file "research")
			       (:file "query-views")
			       (:file "search-bar")
			       (:file "explorer-pop-widget")
			       (:file "data-dict")
			       (:file "explorer"))
			      :depends-on (collect analytics)
			      :serial t)
		     (:module reporting
			      :components
			      ((:file "report")
			       (:file "summary"))
			      :depends-on (analytics)
			      :serial t)
                     (:module uploads
                              :components
                              ((:file "uploads")
                               (:file "upload-ctrl"))
                              :serial t)
		     (:module patient-home
			      :components
			      ((:file "user-controls")
			       (:file "lam-tv-widget")
			       (:file "patient-home"))
			      :depends-on (collect explorer forums create admin
						   "user-map-widget" blog)
			      :serial t)
		     (:module clinician-home
			      :components
			      ((:file "choose-center")
                               (:file "choose-patient")
                               (:file "patient-editor")
                               (:file "center-editor")
                               (:file "clinician-editor")
			       (:file "clinician-home"
                                      :depends-on ("choose-center"
                                                   "choose-patient"
                                                   "patient-editor"
                                                   "center-editor"
                                                   "clinician-editor"))))
		     )
		    :depends-on (core data-model)))
		 :depends-on ("package" conf)
		 )
		 (:module patches
			  :components
			  ((:file "patches")
			   (:module lamsight-1.5
				    :components
				    ((:file "2010-05-08-266-lamsight-study-recruitment")))
			   (:module ilr-1.5
				    :components
				    ((:file "2010-05-16-288-ilr-message-templates"))))
			  :serial t
			  :depends-on ("package")))
    :depends-on (:weblocks :stdutils :langutils :ironclad 
			   :cl-l10n :local-time :parse-number
			   :cl-smtp :cl-twitter :cl-markdown
			   :weblocks-elephant 
			   :drakma :montezuma))


(defsystem registry-flashex 
  :name "New Registry Explorer"
  :version "0.1"
  :maintainer "Ian Eslick"
  :components ((:module src
	       :components
	       ((:module plugins
                :components
		((:module flashex
		 :components
		 ((:file "package")
		  (:file "listserv")
		  (:file "cgraph")
		  (:file "concept-graph")
		  (:file "flashex-test")
		  (:file "charting")
		  (:file "charts")
		  (:file "flashex"))
		 :serial t))))))
  :depends-on (:registry :smart :cl-mathstats))
