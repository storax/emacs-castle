;;; init-fill-column-indicator --- Show the "soon-someone's-gonna-pep8ify-me-zone"

;;; Commentary:

;;; Code:
(require 'init-elpa)
(require-package 'fill-column-indicator)
(require 'fill-column-indicator)

(setq fci-rule-width 2
      fci-rule-column 90)

(add-hook 'python-mode-hook 'fci-mode)

(provide 'init-fill-column-indicator)
;;; init-fill-column-indicator ends here
