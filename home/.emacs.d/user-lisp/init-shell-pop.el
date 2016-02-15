;;; init-shell-pop --- Shell popup

;;; Commentary:

;;; Code:
(require 'init-elpa)
(require-package 'shell-pop)
(require 'shell-pop)

(custom-set-variables
 '(shell-pop-default-directory nil)
 '(shell-pop-shell-type (quote ("multi-term" "*zsh*" (lambda nil (multi-term)))))
 '(shell-pop-universal-key "<f10>")
 '(shell-pop-window-size 60)
 '(shell-pop-full-span t)
 '(shell-pop-window-position "bottom"))

(provide 'init-shell-pop)
;;; init-shell-pop ends here
