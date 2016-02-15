;;; init-magit --- Configure magit

;;; Commentary:

;;; Code:
(require 'init-elpa)
(require-package 'magit)
(require-package 'magit-gitflow)
(require 'magit)
(require 'magit-gitflow)

;;Magit
(global-set-key (kbd "C-x g") 'magit-status)

(add-hook 'magit-mode-hook 'turn-on-magit-gitflow)

(provide 'init-magit)
;;; init-magit ends here
