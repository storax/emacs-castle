;;; init-folding --- Configure folding

;;; Commentary:

;;; Code:
(require 'init-elpa)
(require-package 'fold-dwim)
(require 'fold-dwim)

(global-set-key (kbd "C-c C-h") 'fold-dwim-hide-all)
(global-set-key (kbd "C-c TAB") 'fold-dwim-toggle-selective-display)
(global-set-key (kbd "C-c C-b") 'fold-dwim-toggle)
(global-set-key (kbd "C-c C-e") 'fold-dwim-show-all)

(provide 'init-folding)
;;; init-folding ends here
