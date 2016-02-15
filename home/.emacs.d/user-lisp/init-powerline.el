;;; init-powerline --- Fancy modeline

;;; Commentary:

;;; Code:
(require 'cl-lib)
(require 'init-elpa)
(require 'init-magit)
(require 'init-spotify-el)
(require 'storax-icons)
(require 'pyvenv)
(require-package 'powerline)
(require 'powerline)
(require 'rng-valid)
;;----------------------------------------------------------------------------
;; Face customization
;;----------------------------------------------------------------------------
(defface flycheck-color-mode-line-error-face-active
  '((t :foreground "#efefef" :weight normal :background "#990A1B"))
  "Face for the modeline in buffers with Flycheck errors."
  :group 'flycheck-faces)

(defface flycheck-color-mode-line-warning-face-active
  '((t :foreground "#efefef" :weight normal :background "#7B6000"))
  "Face for the modeline in buffers with only Flycheck warnings."
  :group 'flycheck-faces)

(defface flycheck-color-mode-line-info-face-active
  '((t :foreground "#efefef" :weight normal :background "#00629D"))
  "Face for the modeline in buffers with only Flycheck info."
  :group 'flycheck-faces)

(defface flycheck-color-mode-line-error-face-inactive
  '((t :foreground "#efefef" :weight normal :background "#B22222"))
  "Face for the modeline in buffers with Flycheck errors."
  :group 'flycheck-faces)

(defface flycheck-color-mode-line-warning-face-inactive
  '((t :foreground "#efefef" :weight normal :background "#8B7000"))
  "Face for the modeline in buffers with only Flycheck warnings."
  :group 'flycheck-faces)

(defface flycheck-color-mode-line-info-face-inactive
  '((t :foreground "#efefef" :weight normal :background "#0072AD"))
  "Face for the modeline in buffers with only Flycheck info."
  :group 'flycheck-faces)

;;----------------------------------------------------------------------------
;; Custom Functions for images
;;----------------------------------------------------------------------------
(defun storax/string-from-file (file)
  "Read FILE."
  (with-temp-buffer (insert-file-contents file) (buffer-string)))

(defun storax/color-svg (image color1)
  "Hacky stuff.  The svgs have %s for fill color.

IMAGE is the svg as string.
COLOR1 is the color to apply."
  (format image color1))

(cl-defun storax/create-image (img color1 &optional (acc 90))
  "Creates a image out of the a image data and colors it.
  ascent 90 seems to work best. mask is for transparent background"
  (create-image (storax/color-svg img color1) nil t :ascent acc :mask 'heuristic))

(cl-defun storax/dz-create-image-plain (file &optional (acc 85))
  "Creates a image.
  mask is for transparent background"
  (create-image (storax/string-from-file file) nil t :ascent acc :mask 'heuristic))

(defun storax/create-image-with-face (image face)
  "Color IMAGE with the foreground color of FACE."
  (storax/create-image image (face-attribute face :foreground nil t)))

(defun storax/powerline-wrap-picture (text img)
  "Propertize TEXT to display IMG."
  (propertize text 'display img))

(cl-defun storax/powerline-picture (text img &optional (acc 85))
  (storax/powerline-wrap-picture text (storax/dz-create-image-plain img acc)))

;; Icons
(defvar storax/github-mark-data (storax/string-from-file "~/.emacs.d/icons/mark-github.svg"))
(defvar storax/bitbucket-mark-data (storax/string-from-file "~/.emacs.d/icons/mark-bitbucket.svg"))
(defvar storax/aqua-left-mesh (storax/powerline-picture "  " "~/.emacs.d/icons/aqua-left-mesh.svg"))
(defvar storax/aqua-right-mesh (storax/powerline-picture "     " "~/.emacs.d/icons/aqua-right-mesh.svg"))
(defvar storax/wl-decrypt-success-img (storax/string-from-file "~/.emacs.d/icons/wl-decrypt-success.svg"))
(defvar storax/wl-decrypt-fail-img (storax/string-from-file "~/.emacs.d/icons/wl-decrypt-fail.svg"))
(defvar storax/wl-verify-success-img (storax/string-from-file "~/.emacs.d/icons/wl-verify-success.svg"))
(defvar storax/wl-verify-fail-img (storax/string-from-file "~/.emacs.d/icons/wl-verify-fail.svg"))
(defvar storax/spotify-data (storax/string-from-file "~/.emacs.d/icons/spotify.svg"))
;;(defvar storax/snowflake-left (storax/create-img "~/.emacs.d/icons/snowflake-left.png" "    "))
;;(defvar storax/snowflake-right (storax/create-img "~/.emacs.d/icons/snowflake-right.png" "    "))
(defvar storax/mpc-right (storax/create-img "~/.emacs.d/icons/mpc-right.png" "   "))
(defvar storax/mpc-left (storax/create-img "~/.emacs.d/icons/mpc-left.png" "  "))
(defvar storax/mpc-left-with-text
  (storax/create-img "~/.emacs.d/icons/mpc-left-with-text.png" "        "))

;;----------------------------------------------------------------------------
;; Display bitbucket or github logo
;;----------------------------------------------------------------------------
;; Save the current remote url in each buffer
(defvar storax/remoteurl "")

(defun storax/setremoteurl ()
  "Save the current remote url."
  (make-local-variable 'storax/remoteurl)
  (setq storax/remoteurl (magit-get "remote" "origin" "url")))

;; Kinda like each time we open a file we set the storax/remoteurl
;; then we can display an icon in the modeline accordingly
(add-hook 'after-change-major-mode-hook 'storax/setremoteurl)

;; Github or Bitbucket logo
(defpowerline storax/powerline-remote
  (when (or (string-match "magit" (format "%s" major-mode)) (and (buffer-file-name (current-buffer)) vc-mode))
    (cond ((string-match "github.com" storax/remoteurl)
	   (propertize " " 'display (storax/create-image-with-face storax/github-mark-data face)))
	  ((string-match "bitbucket.org" storax/remoteurl)
	   (propertize " " 'display (storax/create-image-with-face storax/bitbucket-mark-data face))))))

;;----------------------------------------------------------------------------
;; Custom Version Control indicator
;;----------------------------------------------------------------------------
(defun storax/vc-git-working-revision (file)
  "Git-specific version of `vc-working-revision'."
  (let* (process-file-side-effects
         (str (vc-git--run-command-string nil "symbolic-ref" "HEAD"))
	 parsed)
    (vc-file-setprop file 'vc-git-detached (null str))
    (setq parsed
	  (if str
	      (if (string-match "^\\(refs/heads/\\)?\\(.+\\)$" str)
		  (match-string 2 str)
		str)
	    (vc-git--rev-parse "HEAD")))
    (setq deactivate-mark nil)
    parsed))

(defpowerline storax/powerline-vc
  (when (and (buffer-file-name (current-buffer)) vc-mode)
    (if window-system
      (let ((backend (vc-backend (buffer-file-name (current-buffer)))))
	(when backend
	  (if (string= backend "Git")
	      (format " %s %s" (char-to-string #xf020)
		      (storax/vc-git-working-revision (buffer-file-name (current-buffer))))
	    (format " %s:%s" backend
		    (vc-working-revision (buffer-file-name (current-buffer)) backend)))))
      (format-mode-line '(vc-mode vc-mode)))))

;;----------------------------------------------------------------------------
;; Show pyenv
;;----------------------------------------------------------------------------
(defpowerline storax/powerline-pyvenv
  (if pyvenv-virtual-env-name
      (format " (%s)"(file-name-nondirectory (or pyvenv-virtual-env-name "")))))

;;----------------------------------------------------------------------------
;; Show encryption and signature indicators for mails
;;----------------------------------------------------------------------------
(require 'init-wanderlust)
(defpowerline storax/powerline-pgp-decrypt-wl
  (when (equal major-mode 'mime-view-mode)
      (propertize " " 'display (storax/create-image-with-face
      (if storax/wl-decrypt-success
	storax/wl-decrypt-success-img
	storax/wl-decrypt-fail-img)
      face))))

(defpowerline storax/powerline-pgp-verify-wl
  (when (equal major-mode 'mime-view-mode)
      (propertize " " 'display (storax/create-image-with-face
      (if storax/wl-verified-success
	storax/wl-verify-success-img
	storax/wl-verify-fail-img)
		  face))))

(defpowerline storax/powerline-spotify
  (when storax/spotify-connected
      (concat (propertize " " 'display (storax/create-image-with-face storax/spotify-data face)) " ")))
;;----------------------------------------------------------------------------
;; Hide some minor modes
;;----------------------------------------------------------------------------
(require-package 'diminish)
(require 'diminish)
(eval-after-load "whitespace-cleanup-mode" '(diminish 'whitespace-cleanup-mode))
(eval-after-load "helm-mode" '(diminish 'helm-mode))
(eval-after-load "smartparens" '(diminish 'smartparens-mode))
(eval-after-load "drag-stuff" '(diminish 'drag-stuff-mode))
(eval-after-load "hideshow" '(diminish 'hs-minor-mode))
(eval-after-load "highlight-indentation" '(diminish 'highlight-indentation-mode))
(eval-after-load "highlight-indentation" '(diminish 'highlight-indentation-current-column-mode))
(eval-after-load "subword" '(diminish 'subword-mode))
(eval-after-load "projectile" '(diminish 'projectile-mode))
(eval-after-load "spotify.el" '(diminish 'spotify-remote-mode))
(eval-after-load "beacon" '(diminish 'beacon-mode))
(eval-after-load "git-gutter" '(diminish 'git-gutter-mode))

;;----------------------------------------------------------------------------
;; Custom theme with flycheck color
;;----------------------------------------------------------------------------
(defun storax/powerline-center-theme ()
  "Setup a mode-line with major and minor modes centered."
  (interactive)
  (setq-default mode-line-format
		'("%e"
		  (:eval
		   (let* ((active (powerline-selected-window-active))
			  (mode-line (if active 'mode-line 'mode-line-inactive))
			  (face1 (if active 'powerline-active1 'powerline-inactive1))
			  (face2 (if active
				     (cond ((or
					     (flycheck-has-current-errors-p 'error)
					     (and rng-error-count (not (zerop rng-error-count))))
					  'flycheck-color-mode-line-error-face-active)
					 ((flycheck-has-current-errors-p 'warning)
					  'flycheck-color-mode-line-warning-face-active)
					 ((flycheck-has-current-errors-p 'info)
					  'flycheck-color-mode-line-info-face-active)
					 ((flycheck-running-p)
					  'powerline-active2)
					 ('powerline-active2))
				   (cond ((or
					   (flycheck-has-current-errors-p 'error)
					   (and rng-error-count (not (zerop rng-error-count))))
					  'flycheck-color-mode-line-error-face-inactive)
					 ((flycheck-has-current-errors-p 'warning)
					  'flycheck-color-mode-line-warning-face-inactive)
					 ((flycheck-has-current-errors-p 'info)
					  'flycheck-color-mode-line-info-face-inactive)
					 ((flycheck-running-p)
					  'powerline-inactive2)
					 ('powerline-inactive2))))
			  (separator-left (intern (format "powerline-%s-%s"
							  (powerline-current-separator)
							  (car powerline-default-separator-dir))))
			  (separator-right (intern (format "powerline-%s-%s"
							   (powerline-current-separator)
							   (cdr powerline-default-separator-dir))))
			  (lhs (list storax/mpc-left-with-text
				     (powerline-raw "%*" nil 'l)
				     (powerline-buffer-size nil 'l)
				     (powerline-buffer-id nil 'l)
				     (powerline-raw " ")
				     (funcall separator-left mode-line face1)
				     (powerline-narrow face1 'l)
				     (powerline-raw " " face1)
				     (storax/powerline-spotify face1)
				     (storax/powerline-remote face1)
				     (storax/powerline-vc face1)
				     (storax/powerline-pgp-decrypt-wl face1)
				     (storax/powerline-pgp-verify-wl face1)))
			  (rhs (list (powerline-raw global-mode-string face1 'r)
				     (powerline-raw "%4l" face1 'r)
				     (powerline-raw ":" face1)
				     (powerline-raw "%3c" face1 'r)
				     (funcall separator-right face1 mode-line)
				     (powerline-raw " ")
				     (powerline-raw "%6p" nil 'r)
				     storax/mpc-right))
			  (center (list (powerline-raw " " face1)
					(funcall separator-left face1 face2)
					(when (and (boundp 'erc-track-minor-mode) erc-track-minor-mode)
					  (powerline-raw erc-modified-channels-object face2 'l))
					(powerline-major-mode face2 'l)
					(powerline-process face2)
					(powerline-raw ":" face2)
					(powerline-minor-modes face2 'l)
					(storax/powerline-pyvenv face2)
					(powerline-raw " " face2)
					(funcall separator-right face2 face1))))
		     (concat (powerline-render lhs)
			     (powerline-fill-center face1 (/ (powerline-width center) 2.0))
			     (powerline-render center)
			     (powerline-fill face1 (powerline-width rhs))
			     (powerline-render rhs)))))))

;; (powerline-default-theme)
;; (powerline-center-theme)
;; (powerline-center-evil-theme)
;; (powerline-vim-theme)
;; (powerline-nano-theme)
(storax/powerline-center-theme)

(provide 'init-powerline)
;;; init-powerline ends here
