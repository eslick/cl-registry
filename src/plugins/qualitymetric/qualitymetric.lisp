;; -*- Mode:Lisp; tab-width:2; indent-tabs-mode:nil; -*-

;;; Copyright (c) 2008-2010, Massachusetts Institute of;Technology. All rights reserved. 
;;; Copyright (c) 2008-2010, LAM Treatment Alliance. All rights reserved. 
;;; Released under a BSD-style license: http://www.opensource.org/licenses/bsd-license.php 
;;; See LICENSE file 

(in-package :registry)

(define-plugin qualitymetric ()
  )

;;; QualityMetric client 

;; Process flow is as follows:
;; 
;;  1) User browses to /qualitymetric/start and presses Submit (TBD: parameterize survey name)
;;  2) Client form connects to QualityMetric site to login and run survey
;;  3) QualityMetric calls back via GET to /qualitymetric/done and launches survey
;;  4) After survey complete QualityMetric calls back via POST to /qualitymetric/results
;;
;; Components:
;;  1) qualitymetric class to encapsulate query and results
;;  2) widget for form
;;  3) selector and hander methods for URLs to render form, process results, and handle errors

(defvar *qualitymetric-show-results-in-iframe* t)

(defvar *qualitymetric-root-pathname* (make-pathname :directory '(:absolute "qualitymetric")))

(defun qualitymetric-pathname (pathname)
  (merge-pathnames pathname *qualitymetric-root-pathname*))

;;; Request URIs

(defun qualitymetric-page-url (pathname &key
                               (http "http" http-supplied-p)
                               (port (hunchentoot:acceptor-port *weblocks-server*))
                               (address (hunchentoot:acceptor-address *weblocks-server*)))
  (check-type port (integer 80.))
  (let ((httpx (cond
                 ((and (= port 443.) (not http-supplied-p))
                   "https")
                 (http)
                 (t "http"))))
    (format nil "~A://~A~@[:~D~]~A"
            httpx address port
            (puri:uri (qualitymetric-pathname pathname)))))

(defun qualitymetric-request-uri ()
  "URL of page for current request"
  (qualitymetric-page-url (weblocks:request-uri-path)))

(defun qualitymetric-start-page-pathname ()
  (qualitymetric-pathname (make-pathname :directory '(:relative "start"))))

(defun qualitymetric-start-page-url ()
  ;; E.g. "http://192.168.0.12:8080/qualitymetric/start/"
  (qualitymetric-page-url (make-pathname :directory '(:relative "start"))))

(defun qualitymetric-results-page-pathname ()
  (qualitymetric-pathname (make-pathname :directory '(:relative "results"))))

(defun qualitymetric-results-page-url ()
  ;; E.g. "http://192.168.0.12:8080/qualitymetric/results/"
  (qualitymetric-page-url (make-pathname :directory '(:relative "results"))))

(defun qualitymetric-results-page-helper-pathname ()
  (make-pathname :directory '(:absolute "qualitymetric-results-helper")))

(defun qualitymetric-results-page-helper-url ()
  ;; E.g. "/qualitymetric/results-helper/"
  ;; TODO: Fill in http://<address>:<port>
  (qualitymetric-page-url (qualitymetric-results-page-helper-pathname)))

(defun qualitymetric-done-page-pathname ()
  (qualitymetric-pathname (make-pathname :directory '(:relative "done"))))

(defun qualitymetric-done-page-url ()
  ;; E.g. "http://192.168.0.12:8080/qualitymetric/done/"
  (qualitymetric-page-url (make-pathname :directory '(:relative "done"))))

(defun qualitymetric-test-page-pathname ()
  (qualitymetric-pathname (make-pathname :directory '(:relative "test"))))

(defun qualitymetric-test-page-url ()
  ;; E.g. "http://192.168.0.12:8080/qualitymetric/test/"
  (qualitymetric-page-url (make-pathname :directory '(:relative "test"))))

;;; Connect to QualityMetric server
;;
;; Encapsulate everything we need to connect to QualityMetric site and get survey results

(defclass qualitymetric-connect ()
  ((uri :accessor qmconn-uri :initarg :uri)
   (referrer :accessor qmconn-referrer :initarg :referrer)
   (survey :accessor qmconn-survey :initarg :survey)
   (group :accessor qmconn-group :initarg :group)
   (login :accessor qmconn-login :initarg :login)))

;;; Set load time global defaults
;;
;; Todo: get initial values from site config params

(defvar *qualitymetric-connect-default-obj*)

(defun qualitymetric-connect-init-defaults (&key uri referrer survey group)
  ;; Parameter values supplied by QualityMetric Corp
  (setq *qualitymetric-connect-default-obj*
        (make-instance 'qualitymetric-connect :uri uri :referrer referrer :survey survey :group group)))

(qualitymetric-connect-init-defaults
 ;; Testing server:
 ;; :uri "https://staging.qualitymetric.com/trustedlogon.asp"
 ;; Production server:
 :uri "https://www.amihealthy.com/trustedlogon.asp"
 ;; Credentials for SF-36 survey licensed to LTA
 :referrer "sbillmeier" :survey 52486. :group 53167.)

(defun make-qualitymetric-connect (&key uri referrer survey group login)
  (let ((qm-defaults (or *qualitymetric-connect-default-obj* (qualitymetric-connect-init-defaults))))
    (check-type qm-defaults qualitymetric-connect)
    (make-instance 'qualitymetric-connect
                   :uri (or uri (qmconn-uri qm-defaults))
                   :referrer (or referrer (qmconn-referrer qm-defaults))
                   :survey (or survey (qmconn-survey qm-defaults))
                   :group (or group (qmconn-group qm-defaults))
                   :login login)))

(defmethod qualitymetric-connect-url ((obj qualitymetric-connect))
  (with-slots (uri referrer) obj
    (format nil "~A?Partner=~A" uri referrer)))

;; POST to server

(defmethod qualitymetric-connect-post ((obj qualitymetric-connect) &key (cookie-jar (make-instance 'drakma:cookie-jar)))
  (multiple-value-bind (result status headers uri stream must-close reason)
      (with-slots (uri referrer survey login group) obj
          (drakma:http-request (qualitymetric-connect-url obj)
                               :method :post :cookie-jar cookie-jar
                               :parameters
                               `(("LoginName" . ,login)
                                 ("GroupID" . ,group)
                                 ("Action" . "1") ;login and run survey
                                 ("SurveyID" . ,(princ-to-string survey))
                                 ,@(if *qualitymetric-show-results-in-iframe* '(("NW" . "1")))
                                 ("OUT" . "3") ;show Member Report at end of survey and return data
                                 ("ErrorURL" . ,(qualitymetric-results-page-url))
                                 ("NB" . ,(qualitymetric-done-page-url)) ;URL to land on after survey
                                 )))
    ;; Returns
    (values result status headers uri stream must-close reason)))

;; Test driver - connect and send POST data request

(defun qmconn-test ()
  (let ((qmconn-test (make-qualitymetric-connect :login "kmcorbett"))
        (cookie-jar (make-instance 'drakma:cookie-jar)))
    (qualitymetric-connect-post qmconn-test :cookie-jar cookie-jar)))

;;; Web app 

;;; Start page - form widget 

(defwidget qualitymetric-start-form ()
  ((connect :accessor qmform-connect :initarg :state :initform nil)))

(defun make-qualitymetric-start-form ()
  (let* ((current-user t)
         (patient (and current-user (current-patient)))
         (login 
          ;; TBD: unique-ify across centers, registry installations
          (and (typep patient 'patient) (id patient)))
         (widget
          (if login
              (make-instance 'qualitymetric-start-form :state (make-qualitymetric-connect :login login))
              (make-widget (f* (with-html (:P :CLASS "qualitymetric-message" "Unable to start survey - no current patient")))))))
    ;; Returns
    widget))

(defun make-qualitymetric-start-page ()
  (make-instance 'composite :widgets (list (make-qualitymetric-start-form))))

(defmethod qualitymetric-start-action ((widget qualitymetric-start-form))
  (let* ((comp (widget-parent widget))
         (sel (widget-parent comp)))
    (declare (ignore sel))
    (mark-dirty comp)                   ;pointless?
    (qualitymetric-connect-url (qmform-connect widget))))

(defmethod render-widget-body ((widget qualitymetric-start-form) &rest args
                               &aux (counter 0.) (connect (qmform-connect widget)))
  (declare (ignore args))
  (with-main-content-area-html
    (:MIDDLE-INDENT
     (let ((debug (get-site-config-param :enable-debugging)))
       (when debug
         (if (> (incf counter) 1)
             (htm (:P (str (format nil "Rendering x~D" counter)))))))
     (with-slots (login group survey) connect
       (cond
         ((null login)
          (with-html (:P "Internal error: No current user")))
         (t
          (with-html
            (:DIV :CLASS "qualitymetric-input"
                  (:FORM :CLASS "qualitymetric-form" :METHOD :POST
                         :ACTION (qualitymetric-start-action widget)
                         :TARGET (if *qualitymetric-show-results-in-iframe* "result" "_self")
                         (:P (str (format nil "Patient: ~A" login)))
                         (:INPUT :NAME "LoginName" :TYPE "hidden" :VALUE login)
                         ;;(:INPUT :NAME "AuxiliaryID" :TYPE "hidden" :VALUE "")
                         (:INPUT :NAME "GroupID" :TYPE "hidden" :VALUE group)
                         (:INPUT :NAME "Action" :TYPE "hidden" :VALUE "1")
                         (:INPUT :NAME "SurveyID" :TYPE "hidden" :VALUE (princ-to-string survey))
                         (:INPUT :NAME "OUT" :TYPE "hidden" :VALUE "3") ;show Member Report at end of survey and return data
                         ;;(:INPUT :NAME "SessionID" :TYPE "hidden" :VALUE "") 
                         ;;(:INPUT :NAME "ReportID" :TYPE "hidden" :VALUE "") 
                         (if *qualitymetric-show-results-in-iframe*
                             (htm (:INPUT :NAME "NW" :TYPE "hidden" :VALUE "1")))
                         (:INPUT :NAME "NB" :TYPE "hidden" :VALUE (qualitymetric-done-page-url))
                         (:INPUT :NAME "ErrorURL"
                                 :TYPE "hidden"
                                 :VALUE (qualitymetric-results-page-url))
                         (:INPUT :NAME "ConfirmExit" :TYPE "hidden" :VALUE "1")
                         (render-button "Start Survey"))))
          (if *qualitymetric-show-results-in-iframe*
              (with-html
                (:DIV :CLASS "qualitymetric-survey" :ALIGN "center"
                      ;; Frame to hold survey window
                      (:IFRAME :SRC "about:blank" :NAME "result" :WIDTH "98%" :ALIGN "center" :HEIGHT "900px"))))))))))

;;; Results page

(defwidget qualitymetric-results-page ()
  ()
  )

(defun qualitymetric-handle-results (&optional patient)
  (with-main-content-area-html
    (:MIDDLE-INDENT
     (:P "Survey results")
     (let ((debug (get-site-config-param :enable-debugging))
           (get-params (hunchentoot::get-parameters*))
           (post-params (hunchentoot::maybe-read-post-parameters :force t)))
       (when debug
         (htm (:P (str (format nil "GET parameters (~D)" (length get-params))))
              (:UL
               (dolist (param get-params)
                 (htm (:LI (str (format nil "~A = ~A" (car param) (cdr param))))))))
         (htm (:P (str (format nil "POST parameters (~D)" (length post-params))))
              (:UL
               (dolist (param post-params)
                 (htm (:LI (str (format nil "~A = ~A" (car param) (cdr param)))))))))
       ;; Get parameters 
       (let ((stat (cdr (assoc "Stat" get-params :test #'string-equal)))
             (desc (cdr (assoc "Desc" get-params :test #'string-equal)))
             (sessions (cdr (assoc "numSessions" post-params :test #'string-equal)))
             (scores (cdr (assoc "NumScores1" post-params :test #'string-equal)))
             (loginName (cdr (assoc "LoginName" post-params :test #'string-equal)))
             (nscores nil))
         (flet ((qm-error (fmt &rest args)
                  (let ((message
                         (concatenate 'string
                                      "Error: In QualityMetric survey processing: "
                                      (apply #'format nil fmt args))))
                    (htm (:P :CLASS "qualitymetric-message"
                             (str message)))
                    (return-from qualitymetric-handle-results (values nil message)))))
           ;; Workaround for bug #327 - losing current-patient when QualityMetric results returned
           (when (null patient)
             (aif (get-patient loginName nil t)
                  (setq patient it)))
           ;; Check return status now
           (cond
             ;; We should be checking status but QM is returning 21 on success not 0 !!
             ((null desc)
              (qm-error "no status returned"))
             ((not (and desc (string-equal desc "Data successfully returned")))
              (qm-error "~A~@[ (~D)~]" stat desc))
             ;; Assertions about sessions: assume only one session
             ((not (string-equal sessions "1"))
              (qm-error "Invalid numSessions = ~S" sessions))
             ;; Assertions about scores
             ((or (not (stringp scores)) (zerop (length scores)))
              (qm-error "Missing NumScores"))
             ((not (typep (setq nscores (ignore-errors (read-from-string scores nil 0.)))
                          '(integer 1)))
              (qm-error "Invalid NumScores: ~A" scores))
             ;; Now check that we have a valid patient
             ((null patient)
              ;; What, still??
              (if loginName
                  (qm-error "invalid LoginName: ~A" loginName)
                  (qm-error "null LoginName")))
             ;; Success!!
             (t
              ;; Loop over questions and get answers
              (loop for num from 1 to nscores
                 as name-param = (format nil "1Name~D" num)
                 as name-value = (cdr (assoc name-param post-params :test #'string-equal))
                 as score-param = (format nil "1Score~D" num)
                 as score-valuestr = (cdr (assoc score-param post-params :test #'string-equal))
                 with score-value
                 with question
                 do (cond
                      ((null name-value)
                       (qm-error "Missing score name: ~A" name-param))
                      ((not (and (stringp name-value) (> (length name-value) 1)))
                       (qm-error "Invalid name value pair: ~A = ~S" name-param name-value))
                      ((null score-valuestr)
                       (qm-error "Missing score value: ~A" score-param))
                      ((not (and (floatp (setq score-value (ignore-errors (read-from-string score-valuestr nil nil))))
                                 (> score-value 0.0)))
                       (qm-error "Invalid score value pair ~A = ~S" score-param score-valuestr))
                      ;; Find matching survey question
                      ((null (setq question (first (get-instances-by-value 'question 'name name-value))))
                       (qm-error "Unrecognized question name: ~A" name-value))
                      ;; Finally
                      (t
                       (htm (:P (str (format nil "~A = ~A" name-value score-value))))
                       (add-answer question patient score-value))))
              ;; Returns
              (return-from qualitymetric-handle-results nscores))))))))
  ;; Returns
  nil)

(defmethod render-widget-body ((widget qualitymetric-results-page) &rest args)
  (declare (ignore args))
  (multiple-value-bind (results message) (qualitymetric-handle-results (current-patient))
    ;; Redirect to results helper page
    (redirect
     (format nil "~A?results=~:[failure~;success~]~@[&message=~A~]"
             (qualitymetric-results-page-helper-url)
             results
             (and message (hunchentoot:url-encode message)))
     :defer ':post-render)))

(defun make-qualitymetric-results-page ()
  (make-instance 'composite :widgets (list (make-instance 'qualitymetric-results-page))))

;;; Done page

(defwidget qualitymetric-done-page ()
  ()
  )

(defmethod render-widget-body ((widget qualitymetric-done-page) &rest args)
  (declare (ignore args))
  (with-main-content-area-html
    (:MIDDLE-INDENT
     (:P "Running survey... Click on a navigation button when done.")
     #+IFWEWANTTOPARSERAWPOSTDATA
     (let ((post-data (hunchentoot:raw-post-data :force-text t)))
       (if (get-site-config-param :enable-debugging)
           (htm (:P (str post-data)))))
     #-IFWEWANTTOPARSERAWPOSTDATA
     (let ((params (hunchentoot::maybe-read-post-parameters :force t)))
       (dolist (param params)
         (htm (:P (str (format nil "~A = ~A" (car param) (cdr param))))))))))

(defun make-qualitymetric-done-page ()
  (make-instance 'composite :widgets (list (make-instance 'qualitymetric-done-page))))

;;; Test page

(defwidget qualitymetric-test-page ()
  ()
  )

(defmethod render-widget-body ((widget qualitymetric-test-page) &rest args)
  (declare (ignore args))
  (with-main-content-area-html
    (:MIDDLE-INDENT
     (with-html
       (:P (str (qualitymetric-request-uri))))
     (:P "Test page - under construction")
     (:P "Test results page:"
         (:FORM :METHOD :POST
                        :ACTION "/qualitymetric/results"
                        (:INPUT :NAME "RESULTS1" :TYPE "hidden" :VALUE "1")
                        (:INPUT :NAME "RESULTS2" :TYPE "hidden" :VALUE "2")
                        (:INPUT :NAME "RESULTS3" :TYPE "hidden" :VALUE "3")
                        (render-button "Submit")))
     (:P "Test done page:"
         (:FORM :METHOD :POST
                        :ACTION "/qualitymetric/done"
                        (:INPUT :NAME "DONE1" :TYPE "hidden" :VALUE "1")
                        (:INPUT :NAME "DONE2" :TYPE "hidden" :VALUE "2")
                        (:INPUT :NAME "DONE3" :TYPE "hidden" :VALUE "3")
                        (render-button "Submit"))))))


(defun make-qualitymetric-test-page ()
  (make-instance 'composite :widgets (list (make-instance 'qualitymetric-test-page))))

;;; Selector to handle /qualitymetric URIs

(defwidget qualitymetric-selector (static-selector)
  ((state :accessor qmsel-state :initform nil)))
    
(defun make-qualitymetric-selector ()
  (let* ((qm-start (make-qualitymetric-start-page))
         (qm-results (make-qualitymetric-results-page))
         (qm-done (make-qualitymetric-done-page))
         (qm-test (make-qualitymetric-test-page))
         (qm-sel (make-instance 'qualitymetric-selector
                                :name "qualitymetric"
                                :panes `(("start" ,qm-start)
                                         ("results" ,qm-results)
                                         ("done" ,qm-done)
                                         ("test" ,qm-test)))))
    (make-instance 'composite :widgets (list qm-sel))))

;;; Handler for helper page

(defun qualitymetric-results-page-helper-handler (request)
  (if (string= (hunchentoot:script-name* request) (namestring (qualitymetric-results-page-helper-pathname)))
      #'(lambda ()
          (let* ((get-params (hunchentoot::get-parameters*))
                 (results (cdr (assoc "results" get-params :test #'string-equal)))
                 (message (cdr (assoc "message" get-params :test #'string-equal))))
            (with-html-to-string
              (:H2 "QualityMetric survey results")
              (:P :CLASS "qualitymetric-message"
                  (:BR)
                  (str
                   (format nil
                           (cond
                             ((string-equal results "failure")
                              "Error: Cannot get survey results~@[: ~A~]")
                             ((string-equal results "success")
                              "Success: Survey results stored~@[: ~A~]")
                             (t
                              "Internal error: Unable to check survey results"))
                           message))))))))

(pushnew 'qualitymetric-results-page-helper-handler hunchentoot::*dispatch-table*)

