
(in-package :registry)

(export '(start-registry stop-registry))

;;
;; Startup and shutdown
;;

(defun start-registry (&rest args &key config debug log-level &allow-other-keys)
  "Starts the application by calling 'start-weblocks' with appropriate arguments
   and then calling all our various inithooks"
  ;; Read the site configuration files
  (read-startup-configuration config)
  (setf debug (or debug (get-site-config-param :enable-debugging)))
  ;; Start weblocks and webapp
  (apply #'start-weblocks (remove-keyword-parameters args :config))
  (apply #'start-webapp 'registry (remove-keyword-parameters args :port :config))
  ;; Customize session parameters
  (setf hunchentoot::*rewrite-for-session-urls* nil) ;; cookies only
  (setf hunchentoot::*session-max-time* #.(* 3 60 60)) ;; 12 hour timeout
  (setf hunchentoot::*session-gc-frequency* 100) ;; 12 hour timeout
  ;; Debugging settings
  (setf *catch-errors-p* t)
  (if debug
      (progn 
	(start-logging (or log-level :debug) weblocks:*weblocks-server*)
	(enable-global-debugging)
        (setf *error-action*
              (case debug
                (:html :html)
                (t :debug))))
      (progn
	(start-logging (or log-level :error) weblocks:*weblocks-server*)
	(disable-global-debugging)
	(setf *error-action* nil)))
  ;; Run hooks
  (funcall-hook :start-app))

(defun stop-registry ()
  "Stops the application by calling 'stop-weblocks'."
  ;; Run hooks
  (funcall-hook :stop-app)
  (stop-logging)
  (stop-webapp 'registry))

(defun reload-registry (&key debug &allow-other-keys)
  "Stop and restart with/without debug setting"
  (stop-registry)
  (start-registry :debug debug))

;; =======================================
;; Define our application
;; =======================================

(defwebapp registry 
  :name "registry"
  :description "Medical Community Registry Platform Webapp"
  :prefix ""
  :init-user-session 'registry::init-user-session
  :public-files-path (namestring (compute-public-files-path :registry))
  :public-files-uri-prefix "pub"
  :autostart nil
  :default-store '*registry-main*
  :bundle-dependency-types '(:stylesheet :script)
  :version-dependency-types '(:stylesheet :script)
  :gzip-dependency-types '()
  :ignore-default-dependencies t
  :dependencies
  `((:stylesheet "layout")
    (:stylesheet "new-main")
    (:stylesheet "dialog")
    (:script "prototype/prototype")
    (:script "prototype/scriptaculous")
    (:script "prototype/builder")
    (:script "prototype/effects")
    (:script "prototype/dragdrop")
    (:script "prototype/lowpro-15")
    (:script "utils/shortcut")
    (:script "weblocks/weblocks")
    (:script "weblocks/dialog")
    ))

;;
;; New session entry point
;;

(defun init-user-session (comp)
  (set-session-language (default-language)) 
  (setf (hunchentoot:session-value 'edited-objects) 
	(make-hash-table))
  (setf (composite-widgets comp)
	(list (make-front-page))))

