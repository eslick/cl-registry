(in-package :registry)

(registry-proclamations)

;;;
;;; Star ratings
;;;

(defclass aggregate-stars-rating ()
  ((average-rating :accessor average-rating :initform nil)
   (number-of-ratings :accessor number-of-ratings :initform 0)))

(defun make-star-string (n)
  (make-string n :initial-element (code-char #x2605)))

(defmethod render-rating ((rating aggregate-stars-rating))
  (with-html
    (:span :class "rating"
	   (str (format nil "~a (of ~d)"
			(make-star-string (average-rating rating))
			(number-of-ratings rating))))))

(defmethod render-rating ((rating integer))
  (with-html
    (:span :class "rating"
	   (str (make-star-string rating)))))

(defmethod render-rating ((rating null))
  (with-html
    (:span :class "rating"
	   (str "no ratings"))))

(defmethod rating-value ((rating null))
  nil)

(defmethod rating-value ((rating integer))
  rating)

(defmethod rating-value ((rating aggregate-stars-rating))
  (average-rating rating))
			 
(defun compute-average-star-rating (object)
  (let* ((users (get-instances-by-class 'user))
	 (sum 0)
	 (n 0))
    (dolist (u users)
      (let* ((ratings (user-ratings u))
	     (r (get-value object ratings)))
	(when r
	  (incf n)
	  (incf sum r))))
    (if (not (zerop n))
      (values (truncate sum n) n)
      nil)))

;;;
;;; Up/down ratings
;;;

; Users can rate a rateable with -1 (down) or 1 (up).
(defpclass rateable-mixin ()
  ((avg-rating :accessor avg-rating :initarg :rating :initform 0)
   (up-ratings :accessor up-ratings :type integer :initform 0)
   (down-ratings :accessor down-ratings :type integer :initform 0)
   (number-of-ratings :accessor number-of-ratings :initform 0)))

(defmethod rate ((object rateable-mixin) user rating)
  (let* ((ratings (user-ratings user))
	 (previous-rating (get-value object ratings)))

    (with-slots (up-ratings down-ratings number-of-ratings) object
      (when previous-rating ;; Remove existing rating
       (decf number-of-ratings)
       (when (< previous-rating 0) (decf down-ratings))
       (when (> previous-rating 0) (decf up-ratings)))
      ;; Add the new rating
      (incf number-of-ratings)
      (when (< rating 0) (incf down-ratings))
      (when (> rating 0) (incf up-ratings))
      (setf (get-value object ratings) rating)))
  (update-avg-rating object))

(defun update-avg-rating (rateable)
  (with-slots (avg-rating number-of-ratings up-ratings down-ratings) rateable
    (setf avg-rating
	  (if (zerop number-of-ratings) 0
	      (truncate (- up-ratings down-ratings) number-of-ratings)))))

(defmethod update-rating-summary ((object rateable-mixin))
  (let* ((users (get-instances-by-class 'user))
	 (number-of-ratings 0)
	 (up-ratings 0)
	 (down-ratings 0))
    ;; Aggregate the rating data
    (dolist (u users)
      (let* ((ratings (user-ratings u))
	     (r (get-value object ratings)))
	(when r
	  (incf number-of-ratings)
	  (if (not (zerop r))
	      (when (> r 0) (incf up-ratings))
	      (when (< r 0) (incf down-ratings))))))
    ;; Update the slots
    (setf (up-ratings object) up-ratings
	  (down-ratings object) down-ratings
	  (number-of-ratings object) number-of-ratings))
  (update-avg-rating object))
