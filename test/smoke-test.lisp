(in-package :registry)

(use-package :selenium)

(defun registry-smoke-test ()
  (with-selenium-session (*selenium-session* "*firefox" "http://www.lamsight.org")
    (do-open "/")
    (do-type "//input[@name='username']" "eslick")
    (do-set-timeout 10)
    (do-click "//li[@id='collect_tab']/a")
    (do-get-value "//li[4]/a")
    (do-click "//form[@id='survey-form']/fieldset/div[1]/div[1]/div[2]/select"
