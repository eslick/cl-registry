(in-package :registry)

;;
;; Provides a generic interface for http-oriented APIs which
;; respond to GET/POST requests with xml/json, etc.
;; 

;;
;; An API handler is a function which takes a list of URI tokens,
;; request arguments, and the current session authentication object
;; and determines what the response data should be.  It should 
;; set the mime type using set-reply-content-type if the 
;; response is not HTML (e.g. json, xml, etc)
;;

(defparameter *api-root-url* "data")

(defvar *api-handlers* (make-hash-table :test #'equal)
  "Register handlers here based on URL root")

(defun register-api-handler (token handler)
  "Register a handler based at token"
  (assert (stringp token))
  (assert (functionp handler))
  (setf (gethash token *api-handlers*) handler))
  
(defun lookup-api-handler (token)
  "Get a handler given the API root token"
  (aif-ret (gethash token *api-handlers*) 
    (setf (hunchentoot:return-code*) hunchentoot:+http-internal-server-error+)
    (hunchentoot:abort-request-handler "API Command Not Found")))

(defun set-reply-content-type (mime-type)
  "To return alternative content"
  (setf (hunchentoot:content-type*) mime-type))

(defmacro define-api-handler (handler-def args &body body)
  (let* ((name (if (symbolp handler-def) handler-def
		   (first handler-def)))
	 (token (if (symbolp name)
		   (string-downcase (symbol-name name))
		   name))
	 (mime (when (consp handler-def) (getf (cdr handler-def) :mime)))
	 (json (when (consp handler-def) (getf (cdr handler-def) :json))))
    `(eval-when (:load-toplevel)
       (register-api-handler 
	,token
	(lambda ,args
	  (declare (ignorable ,@args))
	  ,@(when mime `((setf (hunchentoot::content-type*) ,mime)))
	  ,@body)))))

;;
;; URI based API
;;

(defun dispatch-api-handler (tokens)
  "Given a URL based at *api-root-url*, dispatch to 
   a handler based on the second token with request params
   and the authentication info, if any.  This should be called
   from the top level dispatcher (dashboard, tour, etc)"
  (hunchentoot:abort-request-handler
   (funcall (lookup-api-handler (first tokens))
	    (rest tokens)
	    (request-parameters)
	    (authenticatedp))))

     
;;      
;; Action based API
;;

;; The body of the action request contains the requested content
(define-permanent-action data registry (&rest args &key path &allow-other-keys)
  (let ((tokens (extract-words path)))
    (hunchentoot:abort-request-handler
     (funcall (lookup-api-handler (first tokens))
	      (rest tokens)
	      (remove-keyword-parameters args :path :action)
	      (authenticatedp)))))


