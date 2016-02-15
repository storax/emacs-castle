;;; init-yaml-mode --- Yaml ain't Markup Language

;;; Commentary:

;;; Code:
(require 'init-elpa)
(require-package 'yaml-mode)

(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))

(provide 'init-yaml-mode)
;;; init-yaml-mode ends here
