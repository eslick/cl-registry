;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
(defpackage #:registry-asd
  (:use :cl :asdf))

(in-package :registry-asd)

;; Required by cl-l10n
(setq ccl:*default-file-character-encoding* :utf-8)

(defsystem registry
    :name "Medical Registry"
    :version "0.9"
    :maintainer ""
    :author "MIT Media Lab / Clozure Associates"
    :licence "Soon to be BSD?"
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
			    ((:file "utils")
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
                     (:file "provenance"))
		    :depends-on (core)
		    :serial t)
 		   (:module plugins
		    :components
		    ((:file "twitter")
		     (:file "featured-survey")
		     (:file "import-export")
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
		     (:module admin
			      :components
			      ((:file "admin")
			       (:file "admin-page"))
			      :serial t)
		     (:module forums
			      :components
			      ((:file "forum")
			       (:file "ratings")
			       (:file "recent-topics")
			       (:file "recent-activity")
			       (:file "forums"))
			      :serial t)
		     (:module collect
			      :components
			      ((:file "survey-ctrl"))
			      :depends-on (comments))
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
			       (:file "explorer"))
			      :depends-on (collect analytics)
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
			      ((:file "patient-editor")
			       (:file "clinician-home" :depends-on ("patient-editor"))))
		     )
		    :depends-on (core data-model)))
		 :depends-on ("package" conf)
		 ))
    :depends-on (:weblocks :stdutils :langutils 
			   :ironclad :cl-l10n 
			   :cl-smtp :cl-twitter :cl-markdown
			   :weblocks-elephant 
			   :drakma :montezuma))


