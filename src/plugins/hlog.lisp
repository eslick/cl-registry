(in-package :registry)

(define-api-handler (hlog :mime "text/json") (tokens params auth-p)
  (let ((command (first tokens))
	(body (awhen (hunchentoot::raw-post-data :force-text t)
		(json:decode-json-from-string it))))
    (cond ((equalp command "Authenticate")
	   (prog1 (hlog-authenticate-command body auth-p)
	     (format t "~A~%" (webapp-session-value weblocks::*authentication-key*))))
	  ((not auth-p)
	   (hlog-error :authentication (first tokens)))
	  ((equalp command "GetSurveyUpdates")
	   (hlog-update-surveys-command body params))
	  ((equalp command "UpdateAnswers")
	   (hlog-update-answers-command body))
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
	   ("token" . ,auth-p))))
      (with-html
	(json:encode-json-to-string
	 `(("authenticated" . "false"))))))
	   
(defun hlog-authenticate-p (params)
  (awhen (authenticate-user (assoc-get :username params)
			    (assoc-get :password params))
    (set-session-user it)))


;;
;; Sync with server
;;

(defun hlog-update-surveys-command (message params)
  (declare (ignore message))
  (format t "~A~%" params)
  (if (assoc-get :test params)
      (json:encode-json-to-string *update-response*)
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

;;
;; Testing from client
;;

(defparameter *cookie-jar*
  (make-instance 'drakma:cookie-jar))

(defparameter *update-request*
  '((:*SURVEY\ *LIST (:*SURVEYS ((:+ID+ . "SID1") (:*TYPE . "Dairy") (:*LAST-UPDATED-TIME-STAMP . "12000930")) ((:+ID+ . "SID1") (:*LAST-UPDATED-TIME-STAMP . "12000930")) ((:+ID+ . "SID2") (:*LAST-UPDATED-TIME-STAMP . "12000930"))))))

(defparameter *update-response*
  `((:*SURVEY\ *LIST (:*SURVEYS ((:+ID+ . "SID1") (:*NAME . "Diabetes") (:*TYPE . "Dairy") (:*FETCHED-TIME-STAMP . "12000931") (:*SURVEY-+XML+ . "<SURVEYXML_AS_IN_XML_SCHEMA>")) ((:+ID+ . "SID2") (:*NAME . "Cancer") (:*TYPE . "survey") (:*FETCHED-TIME-STAMP . "12000931") (:*SURVEY-+XML+ . "<SURVEYXML_AS_IN_XML_SCHEMA>"))) (:*OBSOLETE ((:+ID+ . "SID23")) ((:+ID+ . "SID45"))))))

(defun test-hlog-authenticate (user pw)
  (drakma:http-request "http://www.lamsight.org:8081/data/hlog/Authenticate"
		       :method :post
		       :cookie-jar *cookie-jar*
		       :content (json:encode-json-alist-to-string 
				 `((:username . ,user) (:password . ,pw)))
		       :content-type "text/json"))

(defun test-hlog-update (&key (request *update-request*) test-p)
  (drakma:http-request "http://www.lamsight.org:8081/data/hlog/GetSurveyUpdates"
		       :method :post
		       :parameters (when test-p '(("test" . "t")))
		       :cookie-jar *cookie-jar*
		       :content (json:encode-json-alist-to-string request)
		       :content-type "text/json"))