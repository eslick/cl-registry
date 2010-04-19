(in-package :registry)

(define-api-handler (hlog :mime "text/json") (tokens params auth-p)
  (let ((command (first tokens)))
    (cond ((equalp command "Authenticate")
	   (hlog-authenticate-command params auth-p))
	  ((not auth-p)
	   (hlog-error :authentication (first tokens)))
	  ((equalp command "GetSurveyUpdates")
	   (hlog-update-surveys-command params))
	  ((equalp command "UpdateAnswers")
	   (hlog-update-answers-command params))
	  (t (hlog-error :unknown-command (first tokens))))))

(defun hlog-error (type command &optional params)
  (with-html
    (json:encode-json-to-string 
     `(("Error" . ,(format nil "~A error for '~A'" type command))))))

(defun hlog-authenticate-command (params auth-p)
  (if (hlog-authenticate-p params)
      (with-html
	(json:encode-json-to-string
	 `(("authenticated" . "true")
	   ("token" . auth-p))))
      (with-html
	(json:encode-json-to-string
	 `(("authenticated" . "false"))))))
	   
(defun hlog-authenticate-p (params)
  (authenticate-user (assoc-get :username params)
		     (assoc-get :password params)))


;;
;; Sync with server
;;

(defun hlog-update-surveys-command (params)
;;  (hunchentoot:raw-post-data hunchentoot::*request*)
  (with-html
    (json:encode-json-to-string
     `(("Survey List" . ,(get-survey-list params))))))


(defun get-survey-list (params)
  "Given params, return survey for authenticated user"
  (list->array 
   (list (make-survey-entry 1 "Diabetes" "Survey" (get-universal-time)
			    (slurp-file "/usr/local/lamsight/lamsight-1.5-staging/survey.xml")))))

(defun make-survey-entry (id name type timestamp xml)
  "Simple JSON record for a survey"
  `(("ID" . ,id)
    ("Name" . ,name)
    ("Type" . ,type)
    ("LastUpdated" . ,timestamp)
    ("SurveyXML" . ,xml)))

;;
;; Send answers
;;

(defun hlog-update-answers-command (params)
  (with-html
    (if (assoc-get :surveyanswer params)
	"YES"
	"NO")))