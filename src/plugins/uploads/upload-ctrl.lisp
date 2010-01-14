(in-package :registry)

(registry-proclamations)

;; Upload and download files

(define-plugin docs (site-app)
  :tab-name 'docs
  :create 'make-upload-widget)

(defun make-upload-widget (&rest args)
  (declare (ignore args))
  (make-instance 'composite
   :widgets `(,(make-upload-directory-dispatcher)
               )))

(defwidget upload-widget (composite)
  ())

(defmethod render-widget-body ((widget upload-widget) &rest args)
  (declare (ignore args))
  (with-html
    (:p "Hello World")))

;; ============================================================
;; Directory grid
;; ============================================================

(defwidget upload-directory-dispatcher (dispatcher)
  ((grid :accessor upload-directory-dispatcher-grid
         :initform (make-upload-directory-grid))))

(defun make-upload-directory-dispatcher ()
  (make-instance 'upload-directory-dispatcher))

(defmethod get-widget-for-tokens ((widget upload-directory-dispatcher) uri-tokens)
  (let* ((res (upload-directory-dispatcher-grid widget))
         (grid (second (widget-children res)))
         (tokens (remaining-tokens uri-tokens))
         dir)
    (cond ((equal tokens nil)
           (setf dir (hunchentoot:session-value 'upload-directory))
           (unless (or (null dir) (eq dir (get-object (object-id dir))))
             (setf dir nil)))
          (t (setf
              dir
              (loop for toks on tokens
                 for tok = (car toks)
                 for lastdir = nil then dir
                 for dir = (and tok (find-upload-subdir lastdir tok))
                 finally (return dir)
                 when (null dir) do
                   (when (null (cdr toks))
                     (let ((file (find-upload-file lastdir tok)))
                       (when file
                         (let ((path (upload-full-pathname file)))
                           (when (probe-file path)
                             (undo-no-cache)
                             (hunchentoot:handle-static-file path)
                             (request-entirely-handled))))))
                   (return lastdir)))))
    (setf (upload-directory-grid-directory grid) dir
          (hunchentoot:session-value 'upload-directory) dir
          (upload-file-grid-directory (upload-directory-file-grid grid)) dir)
    (pop-tokens uri-tokens (length tokens))
    res))

(defwidget upload-directory-grid (gridedit)
  ((file-grid :accessor upload-directory-file-grid
              :initarg :file-grid
              :initform nil)
   (directory :accessor upload-directory-grid-directory
              :initarg :directory
              :initform nil)))

(defun render-directory-grid-header (grid)
  (with-html
    (:p
     (render-link (f* (goto-root-upload-directory)) #!"Root" )
     (dolist (dir (upload-directory-ancestry
                   (upload-directory-grid-directory grid)))
       (htm " / ")
       (render-link (f* (goto-upload-directory nil dir))
                    (upload-directory-name dir))))
    (:p (:b (str #!"Directories")))))

;; filter-fn isn't passed here. Left it in for when we need to filter by ACL.
(defun upload-directories-query (widget order-by range &key countp filter-fn)
  (let* ((dir (upload-directory-grid-directory widget))
         (dirs (upload-directory-directories dir)))
    (if countp
        (length dirs)
        (weblocks-memory:range-objects-in-memory
         (weblocks-elephant::advanced-order-objects-in-memory
          (if filter-fn
              (weblocks-elephant::filter-objects-in-memory
               dirs filter-fn)
              dirs)
          order-by)
         range))))

(defun can-delete-file-or-directory-p (file-or-dir &optional
                                       (clinician (current-clinician)))
  (check-type file-or-dir (or upload-directory upload-file))
  (check-type clinician clinician)
  (let* ((user (user clinician))
         (admin-p (has-permission-p user :admin))
         (center-admin-p (center-admin-p clinician))
         (creator (creator file-or-dir)))
    (or (null creator)
        (eq user creator)
        admin-p
        (and center-admin-p
             (get-clinician creator (center clinician))))))

(defmethod can-delete-p ((dir upload-directory) &optional
                         (clinician (current-clinician)))
  (and (can-delete-file-or-directory-p dir clinician)
       (every (lambda (x) (can-delete-file-or-directory-p x clinician))
              (upload-directory-files dir))
       (every (lambda (x) (can-delete-p x clinician))
              (upload-directory-directories dir))))
  
(defun delete-upload-directories-confirmation-string (grid items)
  (declare (ignore grid))
  (cond ((every (lambda (id) (can-delete-p (get-object id))) (cdr items))
         "Delete the checked directories, their sub-directories, and all the files?")
        (t (values "You do not have permission to delete one or more of the checked directories or their files!" t))))

(defun delete-upload-directories (widget items)
  (mark-dirty (widget-parent widget))
  (mapc (lambda (id)
          (let ((dir (get-object id)))
            (check-type dir upload-directory)
            ;; Change this to limit transaction size.
            (ensure-transaction () (drop-instance dir))))
        (cdr items)))

(defmethod dataedit-create-new-item-widget :around ((grid upload-directory-grid))
  (let* ((form (call-next-method))
         (dir (dataform-data form)))
    (setf (upload-directory-parent dir) (upload-directory-grid-directory grid))
    form))

(defun render-upload-file-grid-header ()
  (with-html
    (:p (:b (str #!"Documents")))))

(defun make-upload-directory-grid ()
  (let* ((file-grid (make-upload-file-grid))
         (directory-grid
          (make-instance 'upload-directory-grid
                         :file-grid file-grid
                         :data-class 'upload-directory
                         :view 'upload-directory-table-view
                         :item-form-view 'upload-directory-form-view
                         :show-total-items-count-p nil
                         :no-items-to-delete-format-string
                         #!"Please select directories to delete"
                         :delete-confirmation-string-function
                         'delete-upload-directories-confirmation-string
                         :on-delete-items 'delete-upload-directories
                         :allow-drilldown-p t
                         :on-drilldown (cons :do-upload-directory
                                             'goto-upload-directory)
                         :autoset-drilled-down-item-p nil
                         :on-query 'upload-directories-query)))
    (make-instance 'composite
      :widgets
      `(,(lambda () (render-directory-grid-header directory-grid))
         ,directory-grid
         render-upload-file-grid-header
        ,file-grid))))

(defun goto-upload-directory (grid directory)
  (declare (ignore grid))
  (post-action-redirect (format nil "/dashboard/docs/~a"
                                (upload-relative-pathname directory))))

(defun goto-root-upload-directory ()
  (setf (hunchentoot:session-value 'upload-directory) nil)
  (goto-upload-directory nil nil))

(defwidget upload-file-grid (gridedit)
  ((directory :accessor upload-file-grid-directory
              :initarg :directory
              :initform nil)))

;; filter-fn isn't passed here. Left it in for when we need to filter by ACL.
(defun upload-files-query (widget order-by range &key countp filter-fn)
  (let* ((dir (upload-file-grid-directory widget))
         (files (upload-directory-files dir)))
    (if countp
        (length files)
        (weblocks-memory:range-objects-in-memory
         (weblocks-elephant::advanced-order-objects-in-memory
          (if filter-fn
              (weblocks-elephant::filter-objects-in-memory
               files filter-fn)
              files)
          order-by)
         range))))

(defun delete-upload-files-confirmation-string (grid items)
  (declare (ignore grid))
  (cond ((every (lambda (id) (can-delete-file-or-directory-p (get-object id)))
                (cdr items))
         (let ((count (length (cdr items))))
           (if (eql count 1)
               #!"Delete 1 file?"
               (format nil #!"Delete ~a files?" (length (cdr items))))))
        (t (values "You do not have permission to delete one or more of the checked files!" t))))

(defun add-upload-file (grid file)
  (setf (upload-file-directory file) (upload-directory-grid-directory grid)))

(defmethod dataedit-create-new-item-widget :around ((grid upload-file-grid))
  (let* ((form (call-next-method))
         (file (dataform-data form)))
    (setf (upload-file-directory file) (upload-file-grid-directory grid))
    form))

(defun make-upload-file-grid ()
  (make-instance 'upload-file-grid
                 :data-class 'upload-file
                 :view 'upload-file-table-view
                 :item-form-view 'upload-file-form-view
                 :show-total-items-count-p nil
                 :no-items-to-delete-format-string
                 #!"Please select files to delete"
                 :delete-confirmation-string-function
                 'delete-upload-files-confirmation-string
                 ;; :on-delete-items 'delete-upload-files
                 :allow-drilldown-p t
                 :on-drilldown (cons :do-upload-directory 'return-upload-file)
                 :autoset-drilled-down-item-p nil
                 :on-query 'upload-files-query))

(defun return-upload-file (grid file)
  (declare (ignore grid))
  (post-action-redirect (format nil "/dashboard/docs/~a"
                                (upload-relative-pathname file))))
