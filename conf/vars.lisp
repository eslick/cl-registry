(in-package :registry)

(defvar *lamsight-features* nil)
(defvar *enable-scope-explorer* nil)
(defvar *error-action* :log
  ":HTML to let Weblocks handle the error by generating an error page.
   :DEBUG to enter the debugger.
   Anything else to let Hunchentoot handle the error, by logging it, usually.")
