(in-package :registry)

(registry-proclamations)

;; =================================================
;;  Cache visualization datasets for performance
;; =================================================

(defvar *visualization-cache* (make-hash-table))
(defvar *cache-put-count* 0)
(defvar *cache-gc-threshold* 100
  "When to run the reclamation procedure")
(defvar *global-timeout* #.(* 60 60 1)
  "Delete global cached values after 1 hour")

;; Fast hack of weak pointer cache cache; sbcl only

(defun get-cache (key)
  "Get a value from a cache-table."
  (declare (ignore key))
  (error "Uh oh, not implemented..."))
;;  (let ((val (gethash key *visualization-cache*)))
;;    (if val (values (sb-ext:weak-pointer-value val) t)
;;	(values nil nil))))

(defun make-finalizer (key)
  (lambda () (remhash key *visualization-cache*)))

(defun remcache (key)
  (remhash key *visualization-cache*))

(defun setf-cache (key value)
  "Set a value in a cache-table."
  (declare (ignore key value))
  (error))
;;   (let* ((time (get-universal-time))
;; 	 (val (cons value time))
;; 	 (w (sb-ext:make-weak-pointer (cons value time))))
;;     (sb-ext:finalize val (make-finalizer key))
;;     (setf (gethash key *visualization-cache*) w)
;;     (maybe-gc-cache)
;;     value))

(defun maybe-gc-cache ()
  (when (> (incf *cache-put-count*)
	   *cache-gc-threshold*)
    (setf *cache-put-count* 0)
    (let ((now (get-universal-time)))
      (maphash #'(lambda (k v)
		   (destructuring-bind (val . time) v
		     (declare (ignore val))
		     (when (> now (+ time *global-timeout*))
		       (remcache k))))
	       *visualization-cache*))))

(defsetf get-cache setf-cache)

(defun dump-cache ()
  (format t "Dumping cache: ~A~%" *visualization-cache*)
  (maphash #'(lambda (k v) 
		 (format t "key: ~A / value: ~A~%" k v))
	   *visualization-cache*))
