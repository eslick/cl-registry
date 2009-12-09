(in-package :registry)

(defvar *lamsight-api-num* 0)

(defwidget api-handler ()
  ((lock :accessor api-lock :initarg :lock 
	 :initform (bordeaux-threads:make-lock
		    (format nil "lamsight-api-~A" (incf *lamsight-api-num*))))
   (condition :accessor api-condition-var 
	      :initform (bordeaux-threads:make-condition-variable))
   (api-waiting :accessor api-waiting
		:initform nil :affects-dirty-status-p nil)
   (data :accessor api-data :initform nil :affects-dirty-status-p nil)
   (callbacks :accessor api-callbacks :initform nil
	      :initarg :callbacks)))

(defun make-api-handler ()
  "A session-specific AJAX api handler.  Includes a lock for 
   http streaming API calls"
  (let ((handler (make-instance 'api-handler)))
    (setf (hunchentoot:session-value 'api-handler) handler)
    handler))
    
(defun get-api-handler ()
  (hunchentoot:session-value 'api-handler))

(defmethod push-api-command (script (api api-handler))
  "Push a script string onto the data slot"
  (bordeaux-threads:with-lock-held ((api-lock api))
    (push script (api-data api))
    (notify-api-clients api)))

(defmethod push-api-commands (scripts (api api-handler))
  (bordeaux-threads:with-lock-held ((api-lock api))
    (setf (api-data api)
	  (append (reverse scripts) (api-data api)))
    (notify-api-clients api)))

(defun notify-api-clients (api)
  (when (api-waiting api)
    (bordeaux-threads:condition-notify (api-condition-var api))))

(defmethod send-asynch-api-scripts ((api api-handler))
  "Client side requests blocks and then returns all enqueued commands"
  (bordeaux-threads:with-lock-held ((api-lock api))
    (unless (api-data api)
      (setf (api-waiting api) t)
      (bordeaux-threads:condition-wait (api-condition-var api) (api-lock api))
      (setf (api-waiting api) nil))
    (loop for script in (nreverse (api-data api)) do
	 (send-script script))
    (setf (api-data api) nil)))

(defmethod render-widget-body ((api api-handler) &rest args)
  "Simply send any scripts queued on an ajax call"
  (declare (ignore args)))
;;  (assert (ajax-request-p))
;;  (send-asynch-api-scripts api))

(define-permanent-action api-call lamsight2 (&rest args)
  (declare (ignore args))
  (send-asynch-api-scripts (get-api-handler)))

      
;; ====================================================================
;; Flotr with API
;;

(defun make-flotr-test ()
  (make-instance 'flotr-test))

(defwidget flotr-test ()
  ())

(defmethod dependencies append ((widget flotr-test))
;; <!--[if IE]><script language="javascript" type="text/javascript" src="path/to/excanvas.js"></script><![endif]-->
  (list (make-local-dependency :script "excanvas")
;;	(make-local-dependency :script "flotr.debug-0.1.0alpha")
;;	(make-local-dependency :script "base64")
;;	(make-local-dependency :script "canvas2image")
	(make-local-dependency :script "canvastext")
	(make-local-dependency :script "flotr")
	(make-local-dependency :script "processing")))
	

(defmethod render-widget-body ((widget flotr-test) &rest args)
  (declare (ignore args))
  (with-html 
    (:div :id "flotr" :style "width:350px;height:300px;")
    (:canvas :id "canvas" :style "width:350px;height:300px;")
    (:a :href "" :onclick "performAsynchAPIRequest(); return false;"
	"Perform API Test")
    "&nbsp;|&nbsp;"
    (render-link (f* (push-alert-to-queue))
		 "Push alert to queue"))
  (notify-api-clients (get-api-handler)) ;; clear requestors for this api
  (send-script (ps:ps ((@ *event on-ready) loop-asynch-requests))) ;; start polling on load
  (flotr-test '((1 2) (2 3) (3 4)) '((2 3) (3 4) (4 5)))) ;; Queue initial data

(defun push-alert-to-queue ()
  (let ((api (get-api-handler)))
    (push-api-command (ps:ps (alert "Foo")) api)))
	     
;;
;; Flotr test
;;

(defun flotr-test (data1 data2)
  (flet ((push-it (api)
	   (push-api-command 
	    (ps:ps* `((@ *flotr draw) ($ "flotr")
			    (array
			     (create :data ,(lisp-list-to-js-array data1)
				     :label "y = 2x"
				     :lines (create :show "true" :fill "true")
				     :points (create :show "true"))
			     (create :data ,(lisp-list-to-js-array data2)
				     :label "y = 3x"
				     :lines (create :show "true" :fill "true")
				     :points (create :show "true")))))
	    api)))
    (if (boundp 'weblocks::*session*)
	(push-it (get-api-handler))
	(with-session-context 
	  (push-it (get-api-handler))))))

(defun flotr-pie-test ()
  (with-session-context
    (push-api-command
     "var f = Flotr.draw(
		$('flotr'),
		[{data: new PieChart(100,0.2,0.7,0.1).pie(3) , lines: {show: true, fill: true}},
		 {data: new PieChart(100,0.2,0.7,0.1).pie(2) , lines: {show: true, fill: true}},
		 {data: new PieChart(100,0.2,0.7,0.1).pie(1) , lines: {show: true, fill: true}}],
		{yaxis: {max:125,min:-125},xaxis:{max:125,min:-125}});"
     (get-api-handler))))

(defun flotr-pie-test-ps ()
  (with-session-context
    (push-api-command
     (ps:ps* `((@ *flotr draw) ($ "flotr")
		     (array
		      (create :data ((@ ((new *pie-chart) 100 0.2 0.7 0.1) pie) 3)
			      :lines (create :show "true" :fill "true")))
		     (create :yaxis (create :max 125 :min -125)
			     :xaxis (create :max 125 :min -125))))
     (get-api-handler))))

(defun flotr-bar1 ()
  (with-session-context
    (push-api-command
     "var d1 = [];
      var d2 = [];				

      for(var i = 0; i < 4; i++ ){
		d1.push([i,Math.ceil(Math.random()*10)]);
		d2.push([i+0.5, Math.ceil(Math.random()*10)]);
      }

      alert(d1[0]);
				
      Flotr.draw($('flotr'),
		[d1, d2],
		{bars: {show:true, barWidth:0.5},
		 yaxis: {min: 0}
		}
      );"
     (get-api-handler))))


(defun flotr-candlestick1 ()
  (with-session-context
    (push-api-command
      "var d1 = [];
       var price = 3.206;


       for (var i = 0; i < 5; i++) {
             var a = Math.random();
             var b = Math.random();
             var c = (Math.random() * (a + b)) - b;
             d1.push([i, price, price + a, price - b, price + c]);
             price = price + c;
         }
			    
       alert(d1[1]);

       Flotr.draw($('flotr'), [ d1 ], 
                  { 
 		   candles: { show: true, candleWidth: 0.6 },
                   xaxis: {noTicks: 10}
 		  });"
     (get-api-handler))))

(defun flotr-stacked1 ()
  (with-session-context
    (push-api-command
     "var d1 = [], d2 = [], d3 = [];
      for (i = 0; i < 10; i++) {
		  d1[i] = [i, Math.random()];
		  d2[i] = [i, Math.random()];
		  d3[i] = [i, Math.random()];
		}
			
      var f = Flotr.draw(
			$('flotr'), [
			{data:d1, label:'Series 1'},
			{data:d2, label:'Series 2'},
			{data:d3, label:'Series 3'}
			],{
			legend:{
				backgroundColor: '#D2E8FF' // => a light blue background color.
				},
                        HtmlText: false,
			bars: {
			  show: true,
                          stacked: true,
                          //horizontal: true,
                          barWidth: 0.6
			},
			grid: {
			  verticalLines: false
			},
			spreadsheet:{show: true}
		          });"
     (get-api-handler))))



(defun lisp-list-to-js-array (list)
  (cond ((null list)
	 nil)
	((listp list)
	 `(array ,@(mapcar #'lisp-list-to-js-array list)))
	(t list)))

      
(defun flotr-test-2 (data1 data2)
  (declare (ignore data1 data2))
  (with-session-context 
    (push-api-command 
     (ps:ps* `((@ *flotr draw)
		     ($ "flotr")
		     (array (array (array 1 2) (array 2 2))
			    (array (array 1 4) (array 2 5)))
;;		     ,(lisp-list-to-js-array data1))
;;			    ,(lisp-list-to-js-array data2))
		     (create :bars (create :show "true" :bar-width "1.0"))))
     (get-api-handler))))

(defun flotr-test-3 ()
  (with-session-context
    (push-api-command
     (ps:ps* 
      `(progn
	 (let ((p (*processing ($ "canvas"))))
	   ((@ p size) 100 100)
	   ((@ p background) 0)
	   ((@ p fill) 255)
	   ((@ p ellipse) 50 50 50 50))))
     (get-api-handler))))


(defmacro send-papi (tag &body papi-expr)
  `(let ((js (ps:ps* (quote ,(papi-command tag papi-expr)))))
     (with-session-context
       (push-api-command
	js
	(get-api-handler)))
     js))

(defun papi-command-to-js (tag &rest forms)
  (ps:ps* (papi-command tag forms)))

(defmacro papi-command-macro (tag &rest body)
  `(quote ,(papi-command tag body)))

(defun papi-command (tag body)
  `(let ((p (*processing ($ ,tag))))
     ,@(mapcar 'form-to-papi body)))

(defun form-to-papi (form)
  `(,(fn-to-papi (first form)) p
     ,@(rest form)))

(defun fn-to-papi (symbol)
  (intern (concatenate 'string "." (symbol-name symbol))))

#+fixme
(defun papi-test1 ()
  (code-to-papi-command
      (progn
	(size 100 100)
	(background 0)
	(fill 35 35 35)
	(ellipse 50 50 50 50))))

(defun send-processing (tag string)
  (with-session-context
    (push-api-command
     (ps:ps* `(*processing ($ ,tag) ,string))
     (get-api-handler))))

(defun send-processing-file (tag filename)
  (send-processing tag (slurp-file filename)))

;;
;; Low level drawing primitives
;;

(defclass canvas ()
  ((id :accessor canvas-id :initarg :id :initform (error "Must provide an ID"))))

(defmethod canvas-rectangle ((c canvas) x y width height)
  (ps:ps* `(ls-draw-rect ,(canvas-id c) ,x ,y ,width ,height)))

(defmethod canvas-clear ((c canvas))
  (ps:ps* `(ls-clear ,(canvas-id c))))

;; Graphical objects are parameterized.
;; They are implemented by a sequence of drawing commands.
;; We can send these drawing commands 1:1, or 
;; a position and parameters to draw the whole thing
;; thus a low-level implementation exists on both sides

;; A scene graph is a set of objects and parameters which can
;; be interpreted to produce a set of low-level commands

;; Context for primitive actions:
;; - drawing context (fonts, colors, line widths, etc)
;; - coordinate context (transform object-relative to frame)

;; Animations:
;; How to interpolate between two scene graphs
;; Send a scene graph where we define an interpolation strategy
;; for a given object type?

;; Rendering to:
;; - lisp canvas
;; - javascript canvas
;; - pdf
;; - opengl?