(in-package :registry) 

(registry-proclamations)

(defwidget email-unsubscribe-widget (widget)
  ((username :accessor username :initarg :username)
   (type :accessor email-unsubscribe-type :initarg :type))
  (:documentation "Provides a widget that allows users to unsubsribe from periodic emails."))

(defun make-email-unsubscribe-widget (username type &key dom-id)
  (make-instance 'composite :widgets
		 (list (make-instance 'email-unsubscribe-widget
				      :username username
                                      :type type
				      :dom-id dom-id))))

(defmethod render-widget-body ((widget email-unsubscribe-widget) &rest args)
  (declare (ignore args))
  (let* ((username (username widget))
         (user (get-user username))
         (name (and user (user-namestring user)))
         (type (email-unsubscribe-type widget))
         (forums-p (and user (get-preference :forum-subscriber user)))
         (updates-p (and user (get-preference :update-subscriber user))))
    (flet ((process-unsubscribes (&key forums updates unsubscribe &allow-other-keys)
             (when unsubscribe
               (when forums
                 (setf (get-preference :forum-subscriber user) nil))
               (when updates
                 (setf (get-preference :update-subscriber user) nil)))
             (redirect "/")))
      (with-html-form (:post #'process-unsubscribes :use-ajax-p t)
        (:div
         :class "article public-page"
         (:div
          :class "article-body" 
          (:h2 (str #!"Unsubscribe from Email"))
          (cond ((not user)
                 (htm (:p (htm (str #!"Unknown user: ") (str username)))))
                (t
                 (htm
                  (:p (str #!"Welcome ") (str (or name username)))
                  (:p (str #!"Use this page to unsubscribe from email notifications."))
                  (:p (str #!"To subscribe, use the \"Contact\" tab on the \"User Preferences\" dialog."))
                  (:p (cond ((not forums-p)
                             (cond ((not updates-p)
                                    (htm
                                     (str #!"You are not subscribed to emails.")))
                                   (t
                                    (htm
                                     (str
                                      #!"You are not subscribed to forum changes.")))))
                            ((not updates-p)
                             (htm (str #!"You are not subscribed to updates."))))))))
          (:p
           (when (or forums-p updates-p)
             (htm
              (str #!"Unsubscribe from:")
              (:br)
              (when forums-p
                (htm "&nbsp;&nbsp;")
                (render-checkbox "forums" (string-equal type "forums"))
                (htm " " (str #!"forum notifications")
                     (:br)))
              (when updates-p
                (htm "&nbsp;&nbsp;")
                (render-checkbox "updates" (string-equal type "updates"))
                (htm " " (str #!"update notifications")
                     (:br)))
              (render-translated-button "unsubscribe")))
           (render-translated-button "cancel"))))))))
