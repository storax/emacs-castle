;;; init-desktop --- save desktop on save

;;; Commentary:

;;; Code:
(require 'desktop)

(desktop-save-mode 1)

(defun storax/desktop-save ()
  "Save the current desktop to desktop-dirname."
  (interactive)
  ;; Don't call desktop-save-in-desktop-dir, as it prints a message.
  (if (eq (desktop-owner) (emacs-pid))
      (desktop-save desktop-dirname)))

(add-hook 'auto-save-hook 'storax/desktop-save)

(provide 'init-desktop)
;;; init-desktop.el ends here
