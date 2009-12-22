(in-package :flashex)

;; Utility

(defun update-package ()
  (flashex::import-all-registry))

;; Plugin top level

(defwidget flashex (flash-widget)
  ((src :accessor flashex-source :initarg :src)))

(define-plugin flash-explorer (site-app)
  :tab-name 'explore
  :create 'make-explorer)

(defun make-explorer (&rest args)
  (declare (ignore args))
  (make-instance 'flashex 
		 :id "flashex"
		 :name "flash-explorer"
		 :src "/pub/flash/flash-explorer/FlashExplorer.swf"
		 :width 920
		 :height 500))


;; ===============================================
;;  Explorer State
;; ===============================================

(defclass explorer-state ()
  ((view :accessor state-view :initarg :view
	 :documentation "Which pane is visible")))

;; Return the latest UI state on the server
(define-api-handler (fex :mime "text/html") (tokens params auth-p)
  )
  

  
  




	  
  
