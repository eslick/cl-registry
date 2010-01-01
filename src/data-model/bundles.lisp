(in-package :registry)

;;
;; Prototype of container hierarchy for representing
;; the Registry Survey Markup (RSM?:)
;;

;; How to restructure the hierarchy / how to rename?  
;; Import option to retain question names?
;; Different import option to validate identical structure and
;; apply new names?

;; Too many conflicts on container, wrapper, etc.
;; Only conflict on bundle is weblocks bundling, so no sweat


;;
;; A simple container of other containers or questions (leaves)
;;

(defpclass bundle ()
  ((class :accessor class :initarg :class :initform nil)
   (children :accessor children :initarg :children :initform nil))
  (:documentation "A collection of bundles or question"))


(defun map-bundle-tree (fn bundle &optional after)
  "Simple bundle tree walker for collecting information"
  (unless after (funcall fn bundle))
  (map-bundle-tree fn (children bundle))
  (when after (funcall fn bundle)))
;;
;; A generic header mixin
;;

(defpclass header-mixin (translation-mixin)
  ((title :accessor title :initarg :title :initform nil)
   (description :accessor description :initarg :description :initform nil)
   (help :accessor help :initarg :help :initform nil))
  (:documentation "Associates the children with a coherent set of
   content that provides information to the user.  All content is optional"))

(defmethod translate-fields :around ((hb header-mixin))
  (append '(title description help)
	  (call-next-method)))

;;
;; These should probably be made more semantic and less layout oriented,
;; but I want to avoid being too clever for now
;;

(defpclass survey-bundle (bundle header-mixin)
  ()
  (:documentation "A top-level container for surveys"))

(defpclass page-bundle (bundle header-mixin)
  ()
  (:documentation "A container for a set of related groups or questions"))

;;
;; Inline Bundles (replaces rules)
;;

(defpclass trigger-bundle (bundle)
  ((question :accessor inline-question :initarg :question))
  (:documentation "A container which is a question and renders
   none or one of it's children (which must be inline-bundle) 
   based on the current value of the question"))

(defpclass inline-bundle (bundle)
  ((value :accessor inline-value :initarg :value))
  (:documentation "Behaves like a standard bundle except for
   containing the triggering value dictating it's display   
   when it's parent's (a trigger-bundle) question answer matches."))

;;
;; Table bundles
;; 

(defpclass table-bundle (header-bundle translation-mixin)
  ((column-headers :accessor column-headers :initarg :columns :initform nil))
  (:description ""))

(defmethod translate-fields ((tb table-bundle))
  '(column-headers))

(defpclass row-bundle (bundle translation-mixin)
  ((title :accessor title :initarg :title :initform nil))
  (:documentation "Title is an optional field that modifies all questions
   for purposes of indexing and optionally, for layout purposes.  A row
   bundle typically is laid out as a row and/or part of a table.  Its
   a set of closely related questions that should be tightly laid out"))

(defmethod translate-fields ((tb table-bundle))
  '(column-headers))








