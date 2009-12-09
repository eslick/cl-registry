(in-package :registry)

(defparameter *loquendo-language-map*
  '(("en" 6 1)
    ("es" 1 2)
    ("de" 3 3)
    ("fr" 1 4)
    ("pt" 2 6)
    ("it" 3 7)
    ("sv" 1 9)
    ("zh" 1 10)))
;;    ("dt" 11 2))) ;; Dutch



(defmacro with-agent-send-script (() &body exprs)
  `(when (current-user-preference :enable-laura-p)
     (send-script (ps:ps* `(progn ,,@exprs)))))

(defun agent-say-text (text &key (language "en"))
  (aif (assoc language *loquendo-language-map* :test #'equal)
       `(say-text ,text ,@(cdr it) 2)
       `(say-text ,text ,@(cdr (first *loquendo-language-map*)) 2)))

(defparameter *loquendo-emote-tag*
  '((:laugh . "\\_Laugh_02 .")))

(defun agent-emote (tag &key (language "en"))
  (awhen (assoc tag *loquendo-emote-tag*)
    (agent-say-text (cdr it) :language language)))

  
  