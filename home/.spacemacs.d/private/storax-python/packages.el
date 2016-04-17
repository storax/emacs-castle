;;; packages.el --- storax-smartparens layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: David Zuber <zuber.david@gmx.de>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;;; Code:
(defconst storax-python-packages
  '(python pyvenv electric-operator helm) "The list of Lisp packages required by the storax-smartparens layer.")

(defun storax-python/post-init-pyvenv ()
  (use-package pyvenv
    :defer t
    :config
    (defun storax/pyvenv-guess (pyvenvlist)
      "Guess a value in the PYVENVLIST based on the current project."
      (condition-case nil
          (let ((guess (file-name-base (directory-file-name (projectile-project-root)))))
            (if (member guess pyvenvlist)
                guess
              nil))))
    (defun pyvenv-workon (name)
      "Activate the virtual environment names NAME from $WORKON_HOME."
      (interactive
       (list
        (let ((pyvenvlist (pyvenv-virtualenv-list)))
          (completing-read "Work on: " pyvenvlist
                           nil t (storax/pyvenv-guess pyvenvlist) 'pyvenv-workon-history nil nil))))
      (when (not (or (equal name "")
                     ;; Some completion frameworks can return nil for the
                     ;; default, see
                     ;; https://github.com/jorgenschaefer/elpy/issues/144
                     (equal name nil)))
        (pyvenv-activate (format "%s/%s"
                                 (pyvenv-workon-home)
                                 name))))
    (defalias 'workon 'pyvenv-workon)
    (spacemacs/set-leader-keys-for-major-mode 'python-mode
      "vw" 'pyvenv-workon)))

(defun storax-python/post-init-python ()
  (use-package python
    :defer t
    :config
    (add-hook 'python-mode-hook 'storax/set-flycheck-error-function)
    (bind-key "C-c C-p" 'storax/previous-error-wrapped python-mode-map)
    (bind-key "M-n" 'python-nav-forward-block python-mode-map)
    (bind-key "M-p" 'python-nav-backward-block python-mode-map)
    (bind-key "C-c C-o" 'python-helm-occur python-mode-map)
    (spacemacs/set-leader-keys-for-major-mode 'python-mode
      "tt" 'storax/python-test-tox-pytest-runner
      "ta" 'storax/python-test-tox-pytest-runner-all
      "tb" 'storax/python-test-tox-pytest-runner-module
      "tm" 'storax/python-test-tox-pytest-runner-module
      "d SPC" 'gud-break
      "dj" 'gud-jump
      "dr SPC" 'gud-remove-line
      "drb" 'gud-remove-prompt
      "ds" 'gud-step
      "dn" 'gud-next
      "dc" 'gud-cont
      "du" 'gud-up
      "dd" 'gud-down
      "dU" 'gud-until
      "df" 'gud-finish
      "dg" 'storax/pdb-goto-breakpoint
      "db" nil
      "db SPC" 'gud-break
      "dbb" 'python-toggle-breakpoint
      "dbg" 'storax/pdb-goto-breakpoint
      "dbc" 'storax/pdb-clear-breakpoint
      "dbt" 'storax/pdb-toogle-breakpoint
      "dbi" 'storax/pdb-ignore-breakpoint
      "dbc" 'storax/pdb-condition-breakpoint
      "dbc" 'storax/pdb-condition-breakpoint
      "dps" 'storax/pdb-print-symbol
      "dpl" 'storax/pdb-print-line
      "dpr" 'storax/pdb-print-region
      "dpp SPC" 'storax/pdb-pprint-prompt
      "dpps" 'storax/pdb-pprint-symbol
      "dppl" 'storax/pdb-pprint-line
      "dppr" 'storax/pdb-pprint-region
      "dp SPC" 'storax/pdb-print-prompt
      "del" 'storax/pdb-execute-line
      "der" 'storax/pdb-execute-region
      "de SPC" 'storax/pdb-execute-prompt)))

(defun storax-python/init-electric-operator ()
  (use-package electric-operator
    :config
    (defun storax/enclosing-paren ()
      "Return the opening parenthesis of the enclosing parens, or nil
if not inside any parens."
      (interactive)
      (let ((ppss (syntax-ppss)))
        (when (nth 1 ppss)
          (char-after (nth 1 ppss)))))
    (defun storax/python-mode-: ()
      "Handle python dict assignment"
      (if (or (eq (storax/enclosing-paren) ?\{)
              (save-excursion (search-backward-regexp "lambda" (line-beginning-position) t)))
          ": "
        ":"))
    (electric-operator-add-rules-for-mode 'python-mode (cons ":" #'storax/python-mode-:))
    (add-hook 'python-mode-hook #'electric-operator-mode)))

(defun storax-python/post-init-helm ()
  (use-package helm
    :defer t
    :config
    (defun storax/python-helm-occur ()
      "Preconfigured helm for Occur."
      (interactive)
      (helm-occur-init-source)
      (let ((bufs (list (buffer-name (current-buffer)))))
        (helm-attrset 'follow 1 helm-source-occur)
        (helm-attrset 'follow 1 helm-source-moccur)
        (helm-attrset 'moccur-buffers bufs helm-source-occur)
        (helm-set-local-variable 'helm-multi-occur-buffer-list bufs)
        (helm-set-local-variable
         'helm-multi-occur-buffer-tick
         (cl-loop for b in bufs
                  collect (buffer-chars-modified-tick (get-buffer b)))))
      (helm :sources 'helm-source-occur
            :buffer "*helm occur*"
            :history 'helm-occur-history
            :input "^[[:space:]]*\\(def\\|class\\)[[:space:]] "
            :preselect (and (memq 'helm-source-occur helm-sources-using-default-as-input)
                            (format "%s:%d:" (regexp-quote (buffer-name))
                                    (line-number-at-pos (point))))
            :truncate-lines helm-moccur-truncate-lines))))

;;; packages.el ends here
