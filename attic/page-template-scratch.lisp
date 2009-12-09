;; ******************* RENDERING LOGIN WIDGET *************************
;;
;; 	  (:div :class "header-login"
;; 		(aif (current-user)
;; 		     (with-html 
;; 		       (str (format nil "Welcome ~A" (username it)))
;; 		       (render-link #'logout-action "(logout)"))
;; 		     (htm (with-html-form (:get #'simple-login-action)
;; 			    (:label :for "Login" "Username:")
;; 			    (:input :type "text" :name "username")
;; 			    (:label :for "Password" "Password:")
;; 			    (:input :type "password" :name "password")
;; 			    (:input :type "submit" :value "Go")))))


;;		      "Running on <a href=\"http://common-lisp.net/project/cl-weblocks/\">Weblocks</a> and " 
;;		      (server-type) " " 
;;		      (server-version)))))))
;;      (str 
;;       (js:js-script



;; ******************* RENDERING FLAGS AT TOP OF SCREEN *************************
;;
;; (defun render-flags (&rest args)
;;   (declare (ignore args))
;;   (loop for flag-rec in *language-flags* do
;;        (destructuring-bind (lang title file desc) flag-rec
;; 	 (with-html
;; 	   (:li (with-html-form (:post (make-action (lambda (&rest args &key language &allow-other-keys)
;; 						      (declare (ignore args))
;; 						      (setf *language* language)))
;; 				       :class "tb" :use-ajax-p nil)
;; 		    (:input :name "language" :type "hidden" :value (str lang))
;; 		    (:input :type "image" :value "Change" 
;; 			    :src (str (format nil "/pub/images/flags/gif/~A.gif" file))
;; 			    :alt (str desc) :title (str title))))))))
;;	    (:form :class "td" :method "post" :use-ajax-p nil
;;		       :action (make-action (lambda (&rest args &key language &allow-other-keys)
;;					      (format t "Setting language to: ~A" language)))
;;		       :border "4" :bordercolor "#000000"
;;		       (:input :name "language" :type "hidden" :value (str lang))
;;		       (:input :type "image" :value "Change" 
;;			       :src (str (format nil "/pub/images/flags/gif/~A.gif" file))
;;			       :alt (str desc) :title (str title))))))))
  


;; ************************  RENDER NAV SEARCH *********************************
;;	  (when (current-user)
;;	    (htm (:div :class "header-search"
;;		       (render-text-input "search" "" :maxlength "100")))))))
;;	    (:div :class "header-top"
;;		    (:a :class "sitelogo" :href "/" :title "Go to the Home Page")
;; 		    (:div :class "sitename"
;; 			  (:h1 (:a :href "/" :title "Go to the Home Page" "LAMsight"))
;; 			  (:h2 "A global database for the LAM community"))
;; 		    (:style :type "text/css" "<!-- FORM.tb{display:inline; border:4; bordercolor:#000000} -->")
