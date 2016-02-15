;;; .emacs --- Emacs Init File

;;; Commentary:

;; This config is inspired by many others configs.
;; Thanks and sorry I forgot where I got half of the stuff.
;; Some credits i remember:
;; http://www.mygooglest.com/fni/dot-emacs.html
;; https://github.com/purcell/emacs.d
;; Thanks for the input!

;;; Code:

;;----------------------------------------------------------------------------
;; Prerequisites
;;----------------------------------------------------------------------------
(defun storax/log-init-message (msg &rest args)
  "Fabulous formatting of MSG and ARGS."
  (message (format "###* =====[ %s ]===== *###" (apply #'format msg args))))

(storax/log-init-message "Loading my Emacs init file")

;; Personal Info
(setq user-full-name "David Zuber"
      user-mail-address "zuber.david@gmx.de")

;; uptimes
(defvar storax/emacs-load-start-time)
(setq storax/emacs-load-start-time (current-time))

;; turn on Common Lisp support
(require 'cl-lib)  ; provides useful things like `loop' and `setf'

;; Dir with all the actual configs
(add-to-list 'load-path (expand-file-name "user-lisp" user-emacs-directory))
(require 'init-benchmarking) ;; Measure startup time

;;----------------------------------------------------------------------------
;; Temporarily reduce garbage collection during startup
;;----------------------------------------------------------------------------
(defconst storax/gc-cons-threshold (* 20 1000 1000)
  "Initial value of `gc-cons-threshold' at start-up time.")
(setq gc-cons-threshold (* 128 1024 1024))
(add-hook 'after-init-hook
          (lambda () (setq gc-cons-threshold storax/gc-cons-threshold)))

;;----------------------------------------------------------------------------
;; Bootstrapping config
;;----------------------------------------------------------------------------
(storax/log-init-message "Bootstrapping config")
(setq custom-file (expand-file-name ".emacs-custom.el" "~/"))
(require 'init-utils)
;; Package Installing - calls package-initialize
(require 'init-elpa)

;;----------------------------------------------------------------------------
;; Load configs for specific modes and features
;;----------------------------------------------------------------------------
(storax/log-init-message "Loading features")
(require-package 'marmalade)
(require-package 'epc)
(require 'init-basic)
(require 'init-window)
;(require 'init-windmove)
(require 'init-acewindow)
(require 'init-avy)
(require 'init-theme)
(require 'init-fonts)
(require 'init-desktop)
(require 'init-recentf)
(require 'init-ido)
(require 'init-isearch)
(require 'init-popup)
(require 'init-dabbrev)
(require 'init-expand-region)
(require 'init-folding)
(require 'init-multiple-cursors)
(require 'init-drag-stuff)
(require 'init-iedit)
;(require 'init-ace-jump)
(require 'init-helm)
(require 'init-elisp)
(require 'init-smartparens)
(require 'init-whitespace)
(require 'init-yasnippet)
(require 'init-flycheck)
(require 'init-projectile)
(require 'init-elpy)
(require 'init-helm-company)
(require 'init-orgmode)
(require 'init-magit)
(require 'init-powerline)
(require 'init-latex)
(require 'init-yaml-mode)
(require 'init-markdown-mode)
(require 'init-compilation)
(require 'init-helm-dash)
(require 'init-shell-pop)
(require 'init-visual-regexp-steroids)
(require 'init-beacon)
(require 'init-diff-hl)
(require 'init-major-mode-icons)
;(require 'init-minimap)
(require 'init-translate)
(require 'init-spotify-el)
(require 'init-wanderlust)
(require 'init-fill-column-indicator)
(require-package 'git-timemachine)
(require-package 'multi-term)
(require-package 'org)
(require-package 'xkcd)
(require-package 'sx)
(require-package 'json-mode)
(require-package 'free-keys)
(require-package 'rainbow-mode)
(require-package 'dired+)
(require-package 'font-lock+)
(require 'font-lock+)

;;----------------------------------------------------------------------------
;; Allow access from emacsclient
;;----------------------------------------------------------------------------
(storax/log-init-message "Running server")
(require 'server)
(unless (server-running-p)
  (server-start))

;;----------------------------------------------------------------------------
;; Variables customized via the interactive 'customize' interface
;;----------------------------------------------------------------------------
(storax/log-init-message "Loading custom file")
(when (file-exists-p custom-file)
  (load custom-file))

(add-hook 'after-init-hook
          (lambda ()
            (storax/log-init-message "Init completed in %.2fms"
                     (benchmark-init/node-duration-adjusted benchmark-init/durations-tree))))

(provide '.emacs)
;;; .emacs ends here
