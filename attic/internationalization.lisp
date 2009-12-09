(in-package :registry)

(defun initialize-intl ()
  (initialize-l10n))

;; l10n support for time/date, etc.

(defparameter *default-locale* "en_US")
(defparameter *locale-list* 
  '("en_US" "en_GB" "de_DE" 
    "es_US" "es_ES" "ja_JP"))

(defun initialize-l10n ()
  (dolist (locale *locale-list*)
    (cl-l10n:locale locale :use-cache nil))
  (cl-l10n:set-locale *default-locale*))

;; Session setup

;; (defun set-intl (lang country)
;;   (set-locale (lookup-locale ...))
;;   (set-language lang))

  