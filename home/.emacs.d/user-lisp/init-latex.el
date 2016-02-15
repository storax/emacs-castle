;;; init-latex --- For all those thesis stuff

;;; Commentary:

;; Why not org-mode? Shut up! At least not MS-Word.

;;; Code:
(require 'init-elpa)
(require-package 'auctex)
(require-package 'pdf-tools)
(require 'tex)
(require 'tex-mode)
(pdf-tools-install)

;;; Latex PDF mode
(TeX-global-PDF-mode t)
(setq TeX-source-correlate-start-server t)

;;; Key-Bindings
;; Latex umlauts
(defun latex-ae () "Insert ae."
  (interactive) (insert "\\\"a"))
(defun latex-oe () "Insert oe."
  (interactive) (insert "\\\"o"))
(defun latex-ue () "Insert ue."
  (interactive) (insert "\\\"u"))
(defun latex-ss () "Insert sharp s."
  (interactive) (insert "{\\ss}"))

(defun storax/apply-latex-bindings ()
  "Apply latex key bindings.

Why is this a function? And used in a hook?"
  (define-key latex-mode-map (kbd "C-' C-a") 'latex-ae)
  (define-key latex-mode-map (kbd "C-' C-o") 'latex-oe)
  (define-key latex-mode-map (kbd "C-' C-u") 'latex-ue)
  (define-key latex-mode-map (kbd "C-' C-s") 'latex-ss)
)

;;----------------------------------------------------------------------------
;; Hooks
;;----------------------------------------------------------------------------
(add-hook 'pdf-view-mode-hook 'auto-revert-mode)
;; use special hook for tex-default-command, as it is a local variable
(add-hook 'TeX-mode-hook '(lambda () (setq TeX-command-default "latexmk")))
(add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)
(add-hook 'LaTeX-mode-hook 'storax/apply-latex-bindings)

(provide 'init-latex)
;;; init-latex ends here
