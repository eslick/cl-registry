(in-package :registry)

;;
;; Laura
;;


(defwidget laura ()
  ())

(defun make-laura ()
  (make-instance 'laura))

(defmethod dependencies append ((laura laura))
  (list (make-instance 'script-dependency 
;;	 :url "http://vhss-d.oddcast.com/vhost_embed_functions_v2.php?acc=516572&js=1"
	 :url "https://vhost.oddcast.com/vhost_embed_functions_v2.php?acc=516572&js=1"
	 )))

(defmethod render-widget-body ((widget laura) &rest args)
  (declare (ignore args))
  (when (get-preference :enable-laura-p (current-user))
    (with-html
      (:div :id "laura-view"
	    (:script :language "JavaScript" :type "text/javascript" 
		     "Event.onReady( function () { AC_VHost_Embed(516572, 300, 340, 'FFFFFF', 1, 1, 1635490, 1909148, 0, 0, 'a4dbb3dad124e7ee5b0bc31587d02a0a', 8); } );"))
;;      (render-link (f* (say-something))
;;		   "Test speech")
;;      "&nbsp;"
      (render-link (f* (show-hide-laura))
		   "Show/Hide Laura"))))

(defun show-hide-laura ()
  (send-script
   (ps:ps 
     (if (.visible ($ "laura-view"))
	 (.hide ($ "laura-view"))
	 (.show ($ "laura-view"))))))

;;
;; Laura actions
;;

(defun laura-speak (text)
  (with-agent-send-script ()
    (agent-say-text text :language (session-language))))

(defun laura-emote (tag)
  (agent-emote tag))

;;
;; Testing
;;

(defun say-something ()
  (with-agent-send-script ()
    (agent-say-text #!"Welcome to Lam sight.  I am here to help make using Lam sight a friendlier experience." :language (session-language))
;;    (agent-emote :laugh)
    (agent-say-text #!"You can disable me in your options settings.  I will have more to say over time as the site matures." :language (session-language))))

