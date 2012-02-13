(in-package :registry)

(registry-proclamations)

;;
;; This file provides a central set of handlers for 
;; most non-standard HTTP and Lisp events that require
;; site specific behaviors
;;

;;
;; Performance hack
;;
;; Rendering is side-effect free, we can speed it up
;; by using a single read-committed transaction
;;

(defun render-txn-hook (hooks)
  (ensure-transaction (:read-committed t)
    (eval-dynamic-hooks hooks)))

(eval-when (:load-toplevel)
   (pushnew 'render-txn-hook
	   (request-hook :application :dynamic-render)))

;;
;; Expired Actions
;;

(defmethod expired-action-handler ((app registry))
  "Default behavior for private URLs is to redirect to the 
   split-phase login protocol at the current URL.  This way we 
   get back to where we were, even if we couldn't take the action."
  (declare (ignore app))
  (let* ((action (weblocks::get-request-action-name))
	 (permanent-p (when action (weblocks::webapp-permanent-action action))))
    (cond ((and (not permanent-p) (dashboard-url-p))
	   (redirect-to-login (strip-parameters (request-uri-path))))
	  (permanent-p
	   nil)
	  (t 
	   (when hunchentoot::*session*
	     (hunchentoot::remove-session hunchentoot::*session*))
	   (redirect "/")))))

;;
;; Page Not Found 
;;
;;   If not logged in, and is private page, login and continue
;;

(defmethod page-not-found-handler ((app registry))
  "When a page is not found and it is private, login and continue"
  (declare (ignore app))
  (if (and (not (authenticatedp)) (dashboard-url-p))
      (redirect-to-login (request-uri-path))
;;      (if (authenticatedp)
;;	  (redirect "/dashboard/home/")
      (call-next-method)))

(defun dashboard-url-p ()
  "Is this our special private url?"
  (awhen (weblocks::tokenize-uri (weblocks::request-uri*)) 
    (equal (first it) "dashboard")))

(defun redirect-to-login (url)
  "Capture the current URI and redirect to the permanent login action at
   the current webapp's URL so we can run a login flow and then return
   to processing this invalid one"
  (redirect (format nil "~A?action=login&redirect=~A" 
		    (let ((prefix (webapp-prefix))) 
		      (if (equal prefix "") "/" prefix))
		    url)))

(defun strip-parameters (uri)
  (subseq uri 0 (position #\? uri)))

;;
;; Change language handler
;;   
;;   Supports language selection from the page header
;;

(defun change-language-handler (&key language &allow-other-keys)
  (setf (session-language) language)
  (setf (session-string-table) (find-string-table language))
  (funcall-hook :change-language))

;;
;; Error handling
;;
;;   Provide a catch-all error handler so we can log or handle
;;   lisp errors as appropriate
;;

(defmethod handle-client-request :around ((app registry))
  (block nil
    (signal (catch 'registry-error-tag
              (return (call-next-method))))))

(defmethod handle-error-condition ((app registry) c)
  (case *error-action*
    (:debug
     (restart-case (invoke-debugger c)
       (log ()
         :report "Log the error"
         (throw 'registry-error-tag c))
       (html ()
         :report "Generate HTML describing the error"
         (call-next-method))))
    (:html (call-next-method))
    (t (throw 'registry-error-tag c))))

