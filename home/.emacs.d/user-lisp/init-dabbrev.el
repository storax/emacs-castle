;;; init-dabbrev --- Configure dabbrev

;;; Commentary:

;;; Code:
(require 'init-elpa)
(require-package 'dabbrev)

(define-key minibuffer-local-map (kbd "C-<tab>") 'dabbrev-expand)
(global-set-key (kbd "C-<tab>") 'dabbrev-expand)

(provide 'init-dabbrev)
;;; init-dabbrev ends here
