(in-package :registry)

(registry-proclamations)

;; ===========================
;;  Supported Languages
;; ===========================

(defparameter *language-defs*
  `(("English" . "en")
    ("Guoyu" . "zh")                    ;Mandarin Chinese (Putonghua?)
;;    ("Finnish" . "fi")
    ("Français" . "fr")                 ;French
    ("Deutsch" . "de")                  ;German
    ("Italiano" . "it")                 ;Italian
    ("Nihongo" . "ja")                  ;Japanese
    ("Português" . "pt")                ;Portuguese
    ("Español" . "es")                  ;Spanish
;;    ("Norwegian" . "no")
    ("Hebrew" . "il")
;;    ("Korean" . "ko")
;;    ("Russian" . "ru")
;;    ("Romanian" . "ro")
;;    ("German" . "de")
    ("Svensk" . "sv")))                 ;Swedish

(defun valid-languages ()
  *language-defs*)

(defun language-name (code)
  (awhen (find code *language-defs* :key #'cdr :test #'equal)
    (find-translation (car it))))

(defun language-code (name)
  (assoc-get name *language-defs*))

(defparameter *source-language* "en")

;; =============================
;;  Flags 
;; =============================

(defparameter *language-flags*
  '(("de-at" "View site in Austrian" "at" "Austrian Flag")
    ("pt-br" "View site in Brazilian Portugese" "br" "Brazilian Flag")
    ("zh" "View site in Mandarine Chinese" "cn" "Chinese Flag")
    ("en" "View site in English" "gb" "English flag")
    ("fi" "View site in Finnish" "fi" "Finnish Flag")
    ("fr" "View site in French" "fr" "French flag")
    ("de" "View site in German" "de" "German flag")
    ("il" "View site in Hebrew" "il" "Israel Flag")
    ("it" "View site in Italian" "it" "Italian Flag")
    ("ja" "View site in Japanese" "jp" "Japanese Flag")
    ("ro" "View site in Romanian" "ro" "Romanian Flag")
    ("es" "View site in Spanish" "es" "Spanish flag")
    ("de" "View site in German" "ch" "Switzerland\"s Flag")
    ("sv" "View site in Swedish" "se" "Swedish Flag")))

;; =============================
;;  Session language
;; =============================


(define-system-event-hook set-session-language (login)
  "Set the session language based on the new user's preferences"
  (lambda (user)
    (awhen (and user (get-preference :default-language user))
      (change-language-handler :language it))))

(defun default-language ()
  (handler-case
      (aif-ret (session-language)
	       *source-language*)
    (error () *source-language*)))

(defun session-language ()
  (hunchentoot:session-value 'current-language))

(defun set-session-language (lang)
  (setf (hunchentoot:session-value 'current-language) lang))

(defsetf session-language set-session-language)

;; =============================
;;  Translation tables
;; =============================

(defparameter *po-file-directory*
  (registry-relative-path '("intl" "po")))

(defvar *string-tables* (make-hash-table :test #'equal))

(defvar *all-strings* (make-hash-table :test 'equalp))

(defun load-string-table (lang)
  (let ((pairs (group (read-po-file (po-file-pathname lang)) 2))
	(hash (make-hash-table :test 'equalp)))
    (setf pairs (delete "" pairs :test #'equal :key #'second))
    (when pairs
      (hash-populate hash pairs)
      (setf (gethash lang *string-tables*)
	    hash))))

(defun po-file-pathname (lang)
  (merge-pathnames (make-pathname :name lang :type "po")
		   *po-file-directory*))

(defun refresh-string-tables (&optional delete-all-strings)
  (mapc (lambda (lang)
	  (setf (gethash lang *string-tables*)
		(load-string-table lang)))
	(cdrs *language-defs*))
  (when delete-all-strings
    (setf *all-strings* (make-hash-table :test 'equalp)))
  (loop for (nil . session) in (hunchentoot:session-db hunchentoot:*acceptor*)
       do
       (setf (webapp-session-value 'current-string-table session)
             (gethash (webapp-session-value 'current-language session)
                      *string-tables*))))
		   

(defun find-string-table (lang)
  (when lang
    (or (gethash lang *string-tables*)
	(load-string-table lang))))

(defun session-string-table ()
  (ignore-errors 
    (webapp-session-value 'current-string-table)))

(defun set-session-string-table (table)
  (setf (webapp-session-value 'current-string-table) table))

(defsetf session-string-table set-session-string-table)

(defun note-translation-of (string &optional source-file)
  (setf (gethash string *all-strings*)
	(or source-file "#<source unknown>"))
  string)

(defun find-translation (string &optional language)
  (note-translation-of string)
  (aif (or (find-string-table language) (session-string-table))
       (multiple-value-bind (value found)
	   (gethash string it)
	 (if (and value found) value string))
       string))

(defmacro translate (string &environment env)
  (if (and (constantp string env) (stringp string))
      (note-translation-of string (or *compile-file-truename* *load-truename*))
      (warn "non constant string ~S." string))
  `(find-translation ,string))

(defmacro render-translated-button (name &optional (humanize t))
  (check-type name string)
  `(render-button
    ,name
    :value ,(if humanize
                `(humanize-name ,name)
                `(translate ,name))))

;;; use #! for localizable strings

(eval-when (:compile-toplevel :load-toplevel :execute)
  (set-dispatch-macro-character
   #\# #\!
   #'(lambda (stream char arg)
       (declare (ignore char arg))
       (if (eq (peek-char nil stream) #\()
	   (destructuring-bind (id expr) (read stream)
	     (declare (ignore id))
	     `(translate ,expr))
	   `(translate ,(read stream))))))

(defmethod humanize-name :around ((string string))
  (let ((humanized (call-next-method)))
    (find-translation humanized)))

;; ============================================
;;  Write updated PO files from current state
;; ============================================

(defun dump-translations-as-po ()
  (declare (optimize (debug 3)))
  (dolist (lang-def (valid-languages))
    (let* ((lang (cdr lang-def))
	   (string-table (find-string-table lang))
	   (p (merge-pathnames (make-pathname :name lang :type "po")
			       (registry-relative-path '("intl" "po-out")))))
      (with-open-file (po-out p :direction :output :if-exists :supersede
                                :external-format :utf-8)
	(maphash
	 #'(lambda (str loc)
	     (declare (ignore loc)) ;; FIXME: make it a comment.
	     (let ((translated (and string-table (gethash str string-table))))
	       (format po-out "msgid ~s~%" str)
	       (format po-out "msgstr ~s~%~%" (if translated translated ""))))
	 *all-strings*)))))

;; ==============================
;;  Read PO files
;; ==============================

(defun read-po-file (path)
  "Slurp the file into a string"
  (when (probe-file path)
    (parse-po-string 
     (slurp-file path))))

(defun parse-po-string (string)
  (drop-invalid-strings
   (split "[^m]*msgid\\s+\"(.+)\"\\s+msgstr\\s+\"(.*)\""
	  string :with-registers-p t :omit-unmatched-p t :sharedp t)))

(defun drop-invalid-strings (list)
  "Drop the first and then every third string to get rid of
   detritus from the regex extraction"
  (loop for i from 1 
       for elt in (cdr list)
       unless (= 0 (mod i 3))
       collect elt))
 
;;
;; Utils
;;

(defun slurp-file (filename)
  "Reads filename into a string"
  (with-open-file (stream filename)
    (let ((string (make-string (file-length stream))))
      (read-sequence string stream)
     (remove-extra-escapes string))))

(defun remove-extra-escapes (string)
  (regex-replace-all "\\" string ""))

(defun remove-null-strings (list)
  (loop for elt in list 
     unless (equal elt "")
     collect elt))


;;
;; Testing
;;

(defun test-parse ()
  (parse-po-string
   "# foo
    msgid \"Return to forums page\"
    msgstr \"Rückkehr zur Forum-Seite\"

    msgid \"Login\"
    msgstr \"einloggen\"

    msgid \"Return to \\\"forums\\\" page\"
    msgstr \"Rückkehr zur Forum-Seite\""))
    

;; ============================================================
;; Exporting translateable content from user-translation-mixin
;; subclasses.
;; ============================================================


;; Some frobbing is unavoidable, but we may wish to add some sort of
;; line ending canonicalization step when getting string input from
;; the user.
(defun frob-translation-string (istring)
  "Return new string, with #\newline changed to \\n and #\return omitted."
  (let* ((n (count #\newline istring))
	 (len (+ n (length istring)))
	 (ostring (make-array len :element-type 'character :fill-pointer 0)))
    (dotimes (i (length istring) ostring)
      (let ((chr (char istring i)))
        (cond
          ((char= chr #\newline)
           (vector-push #\\ ostring)
           (vector-push #\n ostring))
          ((char= chr #\return)
           nil)
          (t
           (vector-push chr ostring)))))))

(defun unfrob-translation-string (istring)
  (let ((pieces nil)
        (start 0))
    (loop
       for end = (search "\\n" istring :start2 start)
       while end
       do
         (push (cons start end) pieces)
         (setf start (+ end 2)))
    (if (eql 0 start)
        istring
        (let ((ostring (make-string (+ (length pieces)
                                       (reduce #'+ pieces
                                               :key (lambda (x) (- (cdr x) (car x))))
                                      (- (length istring) start))))
              (pos 0))
          (loop
             for (start . end) in (nreverse pieces)
             do
               (replace ostring istring :start1 pos :start2 start :end2 end)
               (incf pos (- end start))
               (setf (aref ostring pos) #\newline)
               (incf pos))
          (replace ostring istring :start1 pos :start2 start)
          ostring))))

(defun print-class-po-entry (object translation)
  (dolist (field (translate-fields object))
    (let ((value (slot-value object field)))
      (etypecase value
        (string
         (format-class-po-entry
          object field value
          (if translation
              (translation-field translation field nil)
              "")))
        (list
         (unless (every (lambda (x)
                          (and (listp x)
                               (stringp (car x))
                               (atom (cdr x))))
                        value)
           (error "Non-alist in field ~s of ~s" field object))
         (let ((translation-value
                (and translation (translation-field translation field nil))))
           (dolist (pair value)
             (let ((translated-string (or (car (rassoc (cdr pair) translation-value
                                                       :test #'equal))
                                          "")))
               (format-class-po-entry
                object field (car pair) translated-string)))))))))

(defun format-class-po-entry (object field value translated-value)
  
  (when (and value (string/= value ""))
    (format t "~&~%# ~s ~s~%" (class-name (class-of object)) field)
    ;; #: is a "reference" marker
    (format t "#: ~s ~s~%" (mid object) field)
    (format t "msgid \"~a\"~%" (frob-translation-string value))
    (format t "msgstr \"~a\"~%" (frob-translation-string translated-value))))

(defvar *published-hash* nil)

(defun init-published-only-hash ()
  (let ((hash (make-hash-table :test 'eq)))
    (labels ((publish-group (group)
               (when group
                 (setf (gethash group hash) t)
                 (dolist (question (group-questions group))
                   (setf (gethash question hash) t))
                 (dolist (rule (group-rules group))
                   (publish-group (group-rule-target rule))))))
      (dolist (survey (find-persistent-objects *registry-main* 'survey))
        (when (published-p survey)
          (dolist (group (survey-groups survey))
            (publish-group group)))))
    (setf *published-hash* hash)))

(defun in-published-hash-p (thing)
  (or (null *published-hash*)
      (gethash thing *published-hash*)))

(defmethod published-p ((thing t))
  (typecase thing
    ((or survey-group question) (in-published-hash-p thing))
    (t t)))

(defun all-translated-p (obj translation)
  (and translation
       (let ((alist (translation-alist translation)))
         (loop
            for field in (translate-fields obj)
            for value = (slot-value obj field)
            for tran = (cdr (assoc field alist :test 'eq))
            when (and value
                      (not (equal value ""))
                      (or (null tran)
                          (equal tran "")
                          (equal tran value)
                          (and (consp value)
                               (loop
                                  for (eng . lisp) in value
                                  for pair = (rassoc lisp tran :test #'equal)
                                  when (or (null pair)
                                           (equal (car pair) "")
                                           (equal (car pair) eng))
                                  return t))))
                            
            return nil
            finally
              (return t)))))

(defun print-class-po-entries (class lang &key
                               published-only-p missing-translations-only-p)
  (let* ((objects (find-persistent-objects *registry-main* class))
         (found-one-p nil))
    (when (and published-only-p
               (not (eq published-only-p :dont-init)))
      (init-published-only-hash))
    (dolist (obj objects)
      (let ((translation (get-translation obj lang)))
        (unless (or (and published-only-p (not (published-p obj)))
                    (and missing-translations-only-p
                         (all-translated-p obj translation)))
          (setf found-one-p t)
          (print-class-po-entry obj translation))))
    found-one-p))

(defun print-class-po-header (filename)
  (format t "~&msgid \"\"~%")
  (format t "msgstr \"\"~%")
  (format t "\"Project-Id-Version: ~a.po 1.0\\n\"
\"PO-Revision-Date: YEAR-MO-DA HO:MI +ZONE\\n\"
\"Last-Translator: FULL NAME <EMAIL@ADDRESS>\\n\"
\"Language-Team: LANGUAGE <LL@li.org>\\n\"
\"MIME-Version: 1.0\\n\"
\"Content-Type: text/plain; charset=utf-8\\n\"
\"Content-Transfer-Encoding: 8bit\\n\"~%~%" filename)
  )

(defparameter *class-names-to-translate* '(blog-entry article survey
					   survey-group question
					   message-template))

(defun class-po-filename (class-name lang)
  (strcat lang "-" (string-downcase class-name)))

(defun po-class-file-pathname (filename &key (direction :out))
  (merge-pathnames (registry-relative-path
                    (ecase direction
                      (:out '("intl" "po-out"))
                      (:in '("intl" "po"))))
                   (make-pathname :name filename :type "po")))

(defun write-class-po-files (lang &key published-only-p missing-translations-only-p)
  (dolist (name *class-names-to-translate*)
    (let* ((filename (class-po-filename name lang))
	   (p (po-class-file-pathname filename))
           (found-one-p nil))
      (unwind-protect
           (with-open-file (po-out p :direction :output :if-exists :supersede
                                   :external-format :utf-8)
             (let ((*standard-output* po-out))
               (print-class-po-header filename)
               (setf found-one-p
                     (print-class-po-entries
                      name lang
                      :published-only-p published-only-p
                      :missing-translations-only-p missing-translations-only-p))
               (when published-only-p
                 (setf published-only-p :dont-init))))
        (unless found-one-p
          (delete-file p))))))

(defstruct class-po-file-entry
  class-name
  id
  slot
  msgid
  msgstr)

(defun read-class-po-file (class-name lang)
  "Read a file written by write-class-po-files, returning a list of
   class-po-file-entry instances."
  (let ((path (po-class-file-pathname (class-po-filename class-name lang)
                                      :direction :in)))
    (with-open-file (stream path :if-does-not-exist nil)
      (when stream
        (read-class-po-stream stream class-name path)))))

(defun read-class-po-stream (stream class-name &optional path)
  (let ((res nil)
        (linecnt 0)
        id slot msgid reading-msgid msgstr reading-msgstr)
    (labels ((err (s) (error "~a in line #~d of ~s" s linecnt path))
             (strip-quotes (s)
               (awhen (and s (length s))
                 (unless (and (>= it 2)
                              (eql #\" (elt s 0))
                              (eql #\" (elt s (1- it))))
                   (err "Malformed quoted value"))
                 (subseq s 1 (1- it))))
             (record-entry ()
               (cond ((and reading-msgstr (not (equal "" msgid)))
                      (push (make-class-po-file-entry
                             :class-name class-name
                             :id id
                             :slot slot
                             :msgid (unfrob-translation-string msgid)
                             :msgstr (unfrob-translation-string msgstr))
                            res))
                     ((or id slot reading-msgid)
                      (error "Missing msgid or msgstr")))
               (setf id nil
                     slot nil
                     msgid nil
                     reading-msgid nil
                     msgstr nil
                     reading-msgstr nil)))
      (loop
         for line = (awhen (read-line stream nil nil)
                      (string-trim-whitespace it))
         for len = (if (null line) (return) (length line))
         for chr1 = (and (> len 0) (elt line 0))
         for words = (split "\\s+" line :limit 2)
         for word1 = (first words)
         for word2 = (second words)
         do
           (incf linecnt)
           (cond ((eql chr1 #\")
                  (let* ((pos (position #\" line :from-end t))
                         (str (and pos (subseq line 1 pos))))
                    (unless pos (err "No closing quote"))
                    (unless (eql pos (1- len))
                      (warn "Garbage after closing quote in line #~d of ~s"
                            linecnt path))
                    (cond (reading-msgid (setf msgid (strcat msgid str)))
                          (reading-msgstr (setf msgstr (strcat msgstr str))))))
                 ((equal word1 "#:")
                  (when (and (or id slot msgid) (not reading-msgstr))
                    (err "Misplaced #:"))
                  (record-entry)
                  (let ((id+slot (split "\\s+" (second words))))
                    (unless (eql 2 (length id+slot))
                      (err "Malformed #:"))
                    (setf id (ignore-errors (parse-integer (first id+slot)))
                          slot (find-symbol (second id+slot) :registry))
                    (unless (integerp id)
                      (err "Record id not an integer"))
                    (unless slot
                      (err "Slot not found"))))
                 ((equal word1 "msgid")
                  (unless word2 (err "Malformed msgid"))
                  (setf word2 (strip-quotes word2))
                  (unless (or (equal word2 "")
                              (and id slot (not reading-msgid) (not reading-msgstr))
                              (err "Misplaced msgid")))
                  (setf msgid word2
                        reading-msgid t))
                 ((equal word1 "msgstr")
                  (unless word2 (err "Malformed msgstr"))
                  (unless reading-msgid (err "Misplaced msgstr"))
                  (unless (or (equal msgid "") (and id slot))
                    (err "Multi-line msgid without id & slot"))
                  (setf word2 (strip-quotes word2))
                  (setf reading-msgid nil
                        msgstr word2
                        reading-msgstr t))))
      (record-entry))
    (nreverse res)))

(defvar *objects-of-class-cache* nil)

(defun find-object-of-class-with-slot (class-name slot value)
  (let ((cache *objects-of-class-cache*))
    (unless (and cache (eq (car cache) class-name))
      (setf cache (cons class-name
                        (find-persistent-objects *registry-main* class-name))
            *objects-of-class-cache* cache))
    (dolist (object (cdr cache))
      (when (equal value (slot-value object slot))
        (return object)))))

(defun update-db-from-class-po-files (lang &key (records-per-transaction 100))
  (let ((total-cnt 0)
        (unfound-cnt 0)
        (untranslated-cnt 0)
        (questionable-cnt 0)
        (bad nil))
    (dolist (class-name *class-names-to-translate*)
      (multiple-value-bind (totcnt unfcnt untcnt quecnt unf que)
          (update-db-from-class-po-file
           class-name lang
           :records-per-transaction records-per-transaction)
        (incf total-cnt totcnt)
        (incf unfound-cnt unfcnt)
        (incf untranslated-cnt untcnt)
        (incf questionable-cnt quecnt)
        (when (or unf que)
          (push (list class-name unf que) bad))))
    (values total-cnt unfound-cnt untranslated-cnt questionable-cnt (nreverse bad))))

(defun update-db-from-class-po-file (class-name lang &key
                                     (records-per-transaction 100))
  "Update the database from a class po file."
  (update-db-from-class-po-entries
   lang
   (read-class-po-file class-name lang)
   :records-per-transaction records-per-transaction))

(defun update-db-from-class-po-entries (lang entries &key
                                        (records-per-transaction 100))
  (let ((total-cnt 0)
        (unfound-cnt 0)
        (untranslated-cnt 0)
        (questionable-cnt 0)
        (unfound nil)
        (questionable nil)
        (last-object nil))
    (loop
       while entries
       do
         (with-transaction ()
           (dotimes (i records-per-transaction)
             (unless entries (return))
             (incf total-cnt)
             (let ((entry (pop entries)))
               (multiple-value-bind
                     (object translation questionable-p untranslated-p)
                   (update-db-from-class-po-file-entry last-object entry lang)
                 (setf last-object object)
                 (cond ((null translation)
                        (cond (untranslated-p
                               (incf untranslated-cnt))
                              (t (incf unfound-cnt)
                                 (push (list (class-po-file-entry-id entry)
                                             (class-po-file-entry-slot entry)
                                             (class-po-file-entry-msgid entry))
                                       unfound))))
                       (questionable-p
                        (incf questionable-cnt)
                        (push (list (class-po-file-entry-id entry)
                                    (mid questionable-p)
                                    questionable-p)
                              questionable))))))))
    (values total-cnt unfound-cnt untranslated-cnt questionable-cnt
            unfound questionable)))

(defun update-db-from-class-po-file-entry (last-object entry lang)
  "Update the database from a single class-po-file-entry instance.
   Returns TRANSLATION instance if successful.
   Returns a second value of true if the record wasn't found at its
   CLASS-PO-FILE-ENTRY-ID."
  (check-type entry class-po-file-entry)
  (let* ((class-name (class-po-file-entry-class-name entry))
         (slot (class-po-file-entry-slot entry))
         (mid (class-po-file-entry-id entry))
         (object (or (and last-object
                          (eql (mid last-object) mid)
                          (typep last-object class-name)
                          last-object)
                     (get-model class-name (class-po-file-entry-id entry))))
         (msgid (class-po-file-entry-msgid entry))
         (translation (class-po-file-entry-msgstr entry))
         (value (and object
                     (typep object class-name)
                     (ignore-errors (slot-value object slot))))
         (questionable-p nil))
    (cond ((and translation (not (equal "" translation)))
           (unless (and value (or (consp value) (equal value msgid)))
             ;; This makes it work in dumped and loaded databases
             ;; where the object IDs have changed.
             (setf object (find-object-of-class-with-slot class-name slot msgid)
                   value msgid
                   questionable-p t))
           (etypecase value
             ((or string null))
             (cons
              (let* ((alist (slot-value-translation object slot lang))
                     (untran-cell (assoc msgid value :test #'equal)))
                (when untran-cell
                  (let ((tran-cell (and untran-cell
                                        (rassoc (cdr untran-cell) alist
                                                :test #'equal))))
                    (if tran-cell
                        (setf (car tran-cell) translation)
                        (push (cons translation (cdr untran-cell)) alist)))
                  (setf translation alist)))))
           (values
            object
            (when object
              (add-translation-fields object lang (list (cons slot translation))))
            (and questionable-p object)))
          (t (values nil nil nil t)))))
