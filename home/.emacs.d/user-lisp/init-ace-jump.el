;;; init-ace-jump --- Configure Ace Jump Mode

;;; Commentary:

;;; Code:
(require 'init-elpa)
(require-package 'ace-jump-mode)

(define-key global-map (kbd "C-c SPC") 'ace-jump-word-mode)
(define-key global-map (kbd "C-c c SPC") 'ace-jump-char-mode)
(define-key global-map (kbd "C-c c c SPC") 'ace-jump-line-mode)

(provide 'init-ace-jump)
;;; init-ace-jump ends here
