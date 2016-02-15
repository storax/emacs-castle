;;; init-projectile --- Configuer projectile

;;; Commentary:

;;; Code:
(require 'init-elpa)
(require-package 'projectile)

(require 'projectile)
(projectile-global-mode)
(setq projectile-completion-system 'helm)

(projectile-register-project-type 'python-tox2 '("tox.ini") nil "tox")

;; Override to support python-tox2 project type as well
(defun projectile-test-prefix (project-type)
  "Find default test files prefix based on PROJECT-TYPE."
  (cond
   ((member project-type '(django python python-tox python-tox2)) "test_")
   ((member project-type '(lein-midje)) "t_")))

(provide 'init-projectile)
;;; init-projectile ends here
