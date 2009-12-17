(in-package :registry)

;; ===========================================================
;; Data provenance
;; 
;; Interface is MAKE-PROVENANCE, GET-PROVENANCE, GET-PROVENANCE-HISTORY
;; plus the PROVENANCE slot accessors.
;; ===========================================================


(registry-proclamations)

(defpclass provenance ()
  ((model :accessor provenance-model :initarg :model :index t)
   (user :accessor provenance-user :initarg :user :initform nil)
   (center :accessor provenance-center :initarg :center :initform nil)
   (time :accessor provenance-time :initarg :time :initform (get-universal-time))))

(defvar *provenance-lock* (elephant-utils:ele-make-lock))

(defmacro with-provenance-lock (&body body)
  `(elephant-utils:ele-with-lock (*provenance-lock*)
     ,@body))

(defvar *provenance-btree* nil)

(defun provenance-btree ()
  (or *provenance-btree*
      (with-provenance-lock
        (or *provenance-btree*
            (setf *provenance-btree*
                  (or
                   (get-from-root '*provenance-btree*)
                   (with-transaction ()
                     (add-to-root '*provenance-btree* (make-btree)))))))))

(defparameter *provenance-history-length*
  nil
  "The default length of history. NIL or <= 0 for none, T for all")

(defparameter *provenance-time-delta* 60
  "Number of seconds between times to consider them close enough to be equivalent")

(defun make-provenance (model &key
                        user center
                        (time (get-universal-time))
                        (history-length *provenance-history-length*))
  (when (and history-length (not (eq history-length t)))
    (check-type history-length (integer 0)))
  (with-provenance-lock
    (with-transaction ()
      (let ((provenance (get-value model (provenance-btree))))
        (cond ((and provenance
                    (eq user (provenance-user provenance))
                    (eq center (provenance-center provenance))
                    (<= (abs (- time (provenance-time provenance)))
                        *provenance-time-delta*))
               provenance)
              (t (let ((history (unless (eq t history-length)
                                  (get-provenance-history model))))
                   (cond ((and history (>= (length history) history-length))
                          (setf history
                                (nthcdr (max 0 (1- (or history-length 0))) history))
                          (let ((prov (car history)))
                            (dolist (p (cdr history)) (drop-instance p))
                            (setf (provenance-user prov) user
                                  (provenance-center prov) center
                                  (provenance-time prov) time)
                            (unless (eq prov provenance)
                              (setf (get-value model (provenance-btree)) prov))
                            prov))
                         (t
                          (let ((prov (make-instance 'provenance
                                                     :model model
                                                     :user user
                                                     :center center
                                                     :time time)))
                            (setf (get-value model (provenance-btree)) prov)))))))))))

                       
(defun get-provenance (model)
  (get-value model (provenance-btree)))

(defun get-provenance-history (model)
  (sort (copy-list (get-instances-by-value 'provenance 'model model))
        #'>
        :key #'provenance-time))
