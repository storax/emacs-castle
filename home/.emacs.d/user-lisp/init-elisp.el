;;; init-elisp -- Configure elisp mode

;;; Commentary:

;;; Code:
(require 'init-utils)

(defun storax/imenu-elisp-sections ()
  "Set settings to display sections in imenu for elisp mode."
  (setq imenu-prev-index-position-function nil)
  (add-to-list 'imenu-generic-expression '("Sections" "^;;;; \\(.+\\)$" 1) t))

;; Make C-x C-e run 'eval-region if the region is active
(defun storax/eval-last-sexp-or-region (prefix)
  "Eval region from BEG to END if active, otherwise the last sexp.

PREFIX is used for 'pp-eval-last-sexp.'"
  (interactive "P")
  (if (and (mark) (use-region-p))
      (eval-region (min (point) (mark)) (max (point) (mark)))
    (pp-eval-last-sexp prefix)))

(global-set-key (kbd "M-:") 'pp-eval-expression)
(after-load 'lisp-mode
  (define-key emacs-lisp-mode-map (kbd "C-x C-e") 'storax/eval-last-sexp-or-region))

(add-hook 'emacs-lisp-mode-hook 'storax/imenu-elisp-sections)

(provide 'init-elisp)
;;; init-elisp ends here
