(in-package :registry)

(registry-proclamations)


;;
;; Fast and dirty google charting API
;;

;; =======================================
;; Chart tools
;; =======================================

(defstruct dataset labels values type)


(defparameter *test-chart* 
  "http://chart.apis.google.com/chart?chs=250x100&chd=t:60,40&cht=p3&chl=Hello|World")

;; =======================================
;;  WIDGET
;; =======================================

(defun make-chart-widget ()
  (make-instance 'chart-widget 
		 :url nil))


(defwidget chart-widget ()
  ((url :accessor chart-url :initform nil :initarg :url)
   (dataset :accessor chart-dataset :initform nil :initarg :dataset)))

(defmethod initialize-instance :after ((widget chart-widget) &rest args)
  (declare (ignore args))
  (setf (chart-url widget) (compute-current-chart widget)))

(defmethod render-widget-body ((chart chart-widget) &rest args)
  (declare (ignore args))
  (with-html
    (:div :class "view"
	  (:p (str (chart-url chart)))
	  (:img :src (chart-url chart)))
    (:div :class "buttons"
	  (render-link (lambda (&rest args)
			 (declare (ignore args))
			 (setf (chart-url chart)
			       (compute-current-chart chart)))
		       "Update Chart")
	  (:hspace)
	  (render-link (lambda (&rest args)
			 (declare (ignore args))
			 (next-dataset chart))
		       "Next Dataset"))))

(defmethod compute-current-chart (chart)
  (if (chart-dataset chart)
      (make-chart (dataset-type (chart-dataset chart)) nil nil (chart-dataset chart))
      (make-chart :pie nil nil (test-dataset))))


(defvar *datasets*
  `(,(make-dataset 
      :type :pie
      :labels '("Hello" "World") 
      :values '(40 60))
    ,(make-dataset
      :type :pie
      :labels '("Hello" "Cruel" "World")
      :values '(20 30 50))))

(defmethod next-dataset (chart)
  (setf (chart-dataset chart)
	(if (eq (chart-dataset chart) (first *datasets*))
	    (second *datasets*)
	    (first *datasets*)))
  (setf (chart-url chart)
	(compute-current-chart chart)))


;; =======================================
;; Pie Charts
;;

(defparameter *chart-defaults* '(("chs" "300x250")))

(defmethod make-chart ((type (eql :pie)) defaults label dataset)
;;  "http://chart.apis.google.com/chart?chs=250x100&chd=t:60,40&cht=p3&chl=Hello|World&")
  (build-chart-url 
   (append (list '("cht" "p"))
           (or defaults *chart-defaults*)
	   (generate-label label)
	   (generate-dataset dataset))))

(defun build-chart-url (list)
  (format nil "http://chart.apis.google.com/chart?~:{~A=~A&~}" list))

(defun generate-label (label)
  (unless (null label)
    `(("chtt" ,(hunchentoot:url-encode label)))))

(defun generate-dataset (dataset)
  `(("chd" ,(format nil "t:~{~A~^,~}" (dataset-values dataset)))
    ("chl" ,(format nil "~{~A~^|~}" (dataset-labels dataset)))))


(defun test-dataset  ()
  (make-dataset :labels '("Hello" "Cruel" "World") :values '(70 10 20)))



