;;; init-yasnippet --- Configure yasnippets

;;; Commentary:

;;; Code:
(require 'init-elpa)
(require-package 'yasnippet)
(require 'yasnippet)
(yas-global-mode 1)
(setq yas-indent-line nil)

;;----------------------------------------------------------------------------
;; use popup menu for yas-choose-value
;;----------------------------------------------------------------------------
(require 'init-popup)

(defun storax/yas-popup-isearch-prompt (prompt choices &optional display-fn)
  "Promt with isearch when there are two yasnippets.

PROMPT is the prompt to use.
CHOICES are a list of available choices.
DISPLAY-FN determines if choice is displayed."
  (when (featurep 'popup)
    (popup-menu*
     (mapcar
      (lambda (choice)
        (popup-make-item
         (or (and display-fn (funcall display-fn choice))
             choice)
         :value choice))
      choices)
     :prompt prompt
     ;; start isearch mode immediately
     :isearch t
     )))

(setq yas-prompt-functions nil)

;;----------------------------------------------------------------------------
;; Hooks
;;----------------------------------------------------------------------------
(add-hook 'term-mode-hook (lambda()
        (setq yas-dont-activate t)))

(provide 'init-yasnippet)
;;; init-yasnippet ends here
