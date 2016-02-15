;;; init-wanderlust --- Configure the mail client

;;; Commentary:

;;; Code:
(require 'init-elpa)
(require-package 'apel)
(require-package 'flim)
(require-package 'semi)
(require-package 'wanderlust)

;; autoload configuration
;; (Not required if you have installed Wanderlust as XEmacs package)
(autoload 'wl "wl" "Wanderlust" t)
(autoload 'wl-other-frame "wl" "Wanderlust on new frame." t)
(autoload 'wl-draft "wl-draft" "Write draft with Wanderlust." t)

;; Directory where icons are placed.
;; Default: the peculiar value to the running version of Emacs.
;; (Not required if the default value points properly)
;(setq wl-icon-directory "~/work/wl/etc")

;; SMTP server for mail posting. Default: 'nil'
;(setq wl-smtp-posting-server "your.smtp.example.com")
;; NNTP server for news posting. Default: 'nil'
;(setq wl-nntp-posting-server "your.nntp.example.com")

(require 'mailcrypt)
;(load-library "mailcrypt") ; provides "mc-setversion"
(mc-setversion "gpg")    ; for PGP 2.6 (default); also "5.0" and "gpg"

(add-hook 'wl-summary-mode-hook 'mc-install-read-mode)
(add-hook 'wl-mail-setup-hook 'mc-install-write-mode)

(defvar storax/wl-decrypt-success nil "Buffer local indicate if decrypted.")
(defvar storax/wl-verified-success nil "Buffer local indicate if verified signature.")
(make-variable-buffer-local 'storax/wl-decrypt-success)
(make-variable-buffer-local 'storax/wl-verified-success)

(defun storax/wl-decrypt-message ()
  "Decrypt the current buffer via mc-decrypt and show errors."
  (interactive)
  (condition-case err
      (let ((r (mc-decrypt)))
	(setq storax/wl-decrypt-success (car r))
	(setq storax/wl-verified-success (cdr r)))
  (error (message "%s" (error-message-string err)))))

(add-hook 'mime-view-mode-hook 'storax/wl-decrypt-message)

;; Nicer window config
(defvar storax/wl-default-initial-folder nil
  "If not nil jump to this folder when using storax/wlr.")
(setq storax/wl-default-initial-folder "%INBOX:\"zuber.david@gmx.de\"@imap.gmx.net:993!")

(defvar storax/wl-window-config-register ?w
  "The register to save the window configuration.")

(defun storax/wlr ()
  "Show the mail client.

Delete other windows, open wanderlust and jump to a folder.
Then open summary and save the window configuration to w"
  (interactive)
  (delete-other-windows)
  (wl)
  (if storax/wl-default-initial-folder
      (wl-folder-jump-folder storax/wl-default-initial-folder))
  (wl-folder-jump-to-current-entity)
  (if storax/wl-window-config-register
      (window-configuration-to-register storax/wl-window-config-register)))

(provide 'init-wanderlust)
;;; init-wanderlust.el ends here
