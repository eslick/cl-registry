(in-package :registry)

(registry-proclamations)

(defmodel upload-directory ()
  ((name :accessor upload-directory-name :initarg :name)
   (parent :accessor upload-directory-parent
           :initarg :parent
           :initform nil
           :index t)
   (creator :accessor creator
            :initarg :creator
            :initform (current-user nil))
   (acl :accessor upload-directory-acl
        :initarg :acl
        :initform nil)))

;; This needs to be changed to limit transaction size
(defmethod drop-instance :before ((dir upload-directory))
  (mapc 'drop-instance (upload-directory-files dir))
  (mapc 'drop-instance (upload-directory-directories dir)))

(defun creator-username (directory)
  (let ((creator (creator directory)))
    (or (and creator (username creator))
        #!"None")))

(defview upload-directory-table-view (:type table
                                      :inherit-from '(:scaffold upload-directory))
  (name)
  (parent :hidep t)
  (creator :reader 'creator-username)
  (acl :hidep t))

(defview upload-directory-form-view (:type form
                                     :inherit-from '(:scaffold upload-directory))
  (name)
  (parent :hidep t)
  (creator :hidep t)
  (acl :hidep t))

(defmodel upload-file ()
  ((directory :accessor upload-file-directory
              :initarg :directory
              :index t)
   (name :accessor upload-file-name
         :initarg :name)
   (creator :accessor creator
            :initarg :creator
            :initform (current-user nil)
            :index t)
   (size :accessor upload-file-size
         :initarg :size)
   (acl :accessor upload-file-acl
        :initarg :acl
        :initform nil)))

(defmethod drop-instance :before ((file upload-file))
  (let ((path (upload-full-pathname file)))
    (when (probe-file path)
      (delete-file path))))

(defun get-upload-directory-root ()
  (or (get-from-root 'upload-directory-root)
      (add-to-root 'upload-directory-root
                   (make-instance 'upload-directory
                                  :name nil
                                  :creator nil))))

(defun ensure-upload-directory (dir)
  (cond ((null dir) (get-upload-directory-root))
        (t (check-type dir upload-directory)
           dir)))

(defun upload-directory-directories (dir)
  (sort
   (get-instances-by-value
    'upload-directory 'parent
    (ensure-upload-directory dir))
   #'string-lessp
   :key #'upload-directory-name))

(defun find-upload-subdir (dir name)
  (find name (upload-directory-directories dir)
        :test #'string-equal
        :key #'upload-directory-name))

(defun check-upload-name (name)
  (check-type name string)
  (map nil
       (lambda (c)
         (when (find c "/:")
           (error "Name contains a bad character: \"~a\"" c) name))
       name))

(defun make-upload-directory (name parent &key (creator nil creator-p) acl)
  (check-upload-name name)
  (setf parent (ensure-upload-directory parent))
  (assert (null (find-upload-subdir parent name)))
  (make-instance 'upload-directory
                 :name name
                 :parent parent
                 :creator (if creator-p creator (current-user t))
                 :acl acl))

(defun upload-directory-files (dir)
  (sort
   (get-instances-by-value
    'upload-file 'directory
    (ensure-upload-directory dir))
   #'string-lessp
   :key #'upload-file-name))

(defun find-upload-file (dir name)
  (find name (upload-directory-files dir)
        :test #'string-equal
        :key #'upload-file-name))

(defun make-upload-file (name directory &key (creator nil creator-p) acl)
  (check-upload-name name)
  (setf directory (ensure-upload-directory directory))
  (assert (null (find-upload-file directory name)))
  (make-instance 'upload-file
                 :name name
                 :directory directory
                 :creator (if creator-p creator (current-user t))
                 :acl acl))

(defmethod upload-directory-ancestry (dir)
  (nreverse
   (loop for parent = (and dir (upload-directory-parent dir))
      while parent
      collect dir
      do (setf dir parent))))

(defmethod upload-relative-pathname ((dir null))
  "")

(defmethod upload-relative-pathname ((dir upload-directory))
  (setf dir (ensure-upload-directory dir))
  (format nil "~{~a/~}" (mapcar 'upload-directory-name
                                (upload-directory-ancestry dir))))

(defmethod upload-relative-pathname ((file upload-file))
  (format nil "~a~a"
          (upload-relative-pathname (upload-file-directory file))
          (upload-file-name file)))

(defun upload-directory-base-path ()
  (registry-relative-path '("uploads")))

(defun upload-full-pathname (file-or-dir)
  (merge-pathnames (upload-relative-pathname file-or-dir)
                   (upload-directory-base-path)))

(defun print-upload-file-or-directory (file-or-dir stream)
  (print-unreadable-object (file-or-dir stream :type t)
    (format stream "oid:~s ~s"
            (object-id file-or-dir)
            (upload-relative-pathname file-or-dir))))

(defmethod print-object ((dir upload-directory) stream)
  (print-upload-file-or-directory dir stream))

(defmethod print-object ((file upload-file) stream)
  (print-upload-file-or-directory file stream))

(defview upload-file-table-view (:type table
                                       :inherit-from '(:scaffold upload-file))
  (directory :hidep t)
  (name)
  (creator :reader 'creator-username)
  (size)
  (acl :hidep t))

(defun upload-file-form-view-reader (obj)
  obj nil)

(defun upload-file-form-view-writer (value obj)
  ;; Value is the file name in the (upload-file-temp-directory)
  ;; Need to put it in a session variable
  ;; And then move it to the proper position in an on-added function
  ;; set up in make-upload-file-grid
  (format *debug-io* "~&upload-file value: ~s~%" value)
  obj nil)

;; This is evaluated at compile time 
(defun upload-file-temp-directory ()
  (registry-relative-path '("uploads-tmp")))

(defview upload-file-form-view (:type form
                                     :inherit-from '(:scaffold upload-file)
                                     :use-ajax-p nil
                                     :enctype "multipart/form-data")
  (directory :hidep t)
  (name :label #!"File"
        :present-as file-upload
        :parse-as (file-upload :upload-directory (upload-file-temp-directory)
                               ;; This could cause collisions.
                               ;; Really need a unique directory per upload,
                               ;; and code to manage that, and clean it
                               ;; up at startup.
                               :file-name :browser))
  (creator :hidep t)
  (size :hidep t)
  (acl :hidep t))
