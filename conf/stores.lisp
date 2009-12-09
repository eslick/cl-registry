
(in-package :registry)

(defun registry-relative-path (dir-list)
  "Computes a path relative to the current defsystem root"
  (namestring
   (merge-pathnames (make-pathname :directory (cons :relative dir-list))
		    (asdf-system-directory :registry))))


(defstore *registry-main* :elephant
  :spec (list :BDB 
	      (registry-relative-path '("data")))
  :args '(:recover t :deadlock-detect t :register nil))
