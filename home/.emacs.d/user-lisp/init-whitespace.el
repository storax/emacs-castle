;;; init-whitespace -- Configure whitespace related stuff

;;; Commentary:

;;; Code:
(require 'init-elpa)
(require-package 'whitespace-cleanup-mode)

(defun storax/trailing-whitespace ()
  "Turn on display of trailing whitespace in this buffer."
  (setq show-trailing-whitespace t))

(dolist (hook '(python-mode-hook
		ruby-mode-hook
		yaml-mode-hook
		rst-mode-hook
		emacs-lisp-mode-hook))
  (add-hook hook #'storax/trailing-whitespace))

;; (defun sanityinc/no-trailing-whitespace ()
;;   "Turn off display of trailing whitespace in this buffer."
;;   (setq show-trailing-whitespace nil))

;; ;; But don't show trailing whitespace in SQLi, inf-ruby etc.
;; (dolist (hook '(special-mode-hook
;;                 Info-mode-hook
;;                 eww-mode-hook
;;                 term-mode-hook
;;                 comint-mode-hook
;;                 compilation-mode-hook
;;                 twittering-mode-hook
;;                 minibuffer-setup-hook))
;;   (add-hook hook #'sanityinc/no-trailing-whitespace))

(global-whitespace-cleanup-mode t)

(global-set-key [remap just-one-space] 'cycle-spacing)

(provide 'init-whitespace)
;;; init-whitespace ends here
