;;; init-elpy --- Configure elpy/python mode

;;; Commentary:

;;; Code:

(require 'init-elpa)
(require-package 'elpy)
;;----------------------------------------------------------------------------
;; Install requirements manually
;;----------------------------------------------------------------------------
(require-package 'company)
(require-package 'find-file-in-project)
(require-package 'highlight-indentation)
(require-package 'pyvenv)
(require-package 'projectile)
(require-package 'electric-operator)
(require-package 'helm)
(require 'projectile)
(require 'elpy)
(elpy-enable)

;; Only use these modules
(setq elpy-modules '(elpy-module-sane-defaults
		     elpy-module-company
		     elpy-module-eldoc
		     elpy-module-highlight-indentation
		     elpy-module-pyvenv
		     elpy-module-yasnippet))

(setq elpy-rpc-backend "rope")

;; Use ipython if available
(when (executable-find "ipython")
  (setq python-shell-interpreter "ipython"))

;;----------------------------------------------------------------------------
;;Python indent the right way
;;----------------------------------------------------------------------------
;;set in python-mode. yas fallback has to be call-other-command.
;;the default python-indent-region sucks IMO
(defun storax/shift-or-indent (&optional arg)
  "'indent-for-tab-command' with ARG or shift with region."
  (interactive "P")
  (if mark-active
      (python-indent-shift-right (region-beginning)
				 (region-end))
    (indent-for-tab-command arg)))

(defun storax/dabbrev-or-indent-left (arg)
  "Shift left with region else use 'dabbrev-expand' with ARG."
  (interactive "*P")
  (if mark-active
      (python-indent-shift-left (region-beginning)
				(region-end))
    (dabbrev-expand arg)))

;;----------------------------------------------------------------------------
;; Test runner
;;----------------------------------------------------------------------------
;;Python testing with tox
(defvar storax/tox-history (list) "History of tox arguments.")

(defun storax/tox (args)
  "Test with tox.

ARGS is a string with arguments for tox."
  (interactive (list (read-string "Tox arguments: " (car storax/tox-history) 'storax/tox-history)))
  (projectile-with-default-dir (projectile-project-root)
    (async-shell-command (format "tox %s" args))))

(defun storax/elpy-test-tox-runner (top file module test)
  "Test the project using tox.

This requires the tox package to be installed.
TOP is the project root.
FILE the test file.
MODULE is the module to test or nil to test all.
TEST is a single test function or nil to test all."
  (interactive (elpy-test-at-point))
  (let (toxargs '(read-string "Tox arguments: " (car storax/tox-history) 'storax/tox-history))
  (projectile-with-default-dir (projectile-project-root)
    (async-shell-command (format "tox %s" toxargs)))))
(put 'storax/elpy-test-tox-runner 'elpy-test-runner-p t)

(defvar storax/pytest-history (list "-vv"))

(defun storax/run-tox-pytest (toxargs pytestargs top file module test)
  "Run tox with pytest.

TOXARGS are the arguments for tox.
PYTESTARGS are the arguments for pytest.
TOP is the project root.
FILE the test file.
MODULE is the module to test or nil to test all.
TEST is a single test function or nil to test all."
  (projectile-with-default-dir (projectile-project-root)
    (cond
     (test
	(async-shell-command (concat
			      (format "tox %s -- py.test %s -k \"%s\" %s "
				      toxargs pytestargs test file))))
     (module
      (async-shell-command (format "tox %s -- py.test %s %s" toxargs pytestargs file)))
     (t
      (async-shell-command (format "tox %s -- py.test %s" toxargs pytestargs))))))

(defun storax/elpy-test-tox-pytest-runner (top file module test)
  "Test the project using tox and pytest.

This requires the tox package to be installed and pytest as test suite in tox.
TOP is the project root.
FILE the test file.
MODULE is the module to test or nil to test all.
TEST is a single test function or nil to test all."
  (interactive (elpy-test-at-point))
  (let ((toxargs (read-string "Tox arguments: " (car storax/tox-history) 'storax/tox-history))
	(pytestargs (read-string "py.test arguments: " (car storax/pytest-history) 'storax/pytest-history)))
  (storax/run-tox-pytest toxargs pytestargs top file module test)))

(defun storax/elpy-test-tox-pytest-runner-default (top file module test)
  "Test the project using tox and pytest.

This requires the tox package to be installed and pytest as test suite in tox.
Call 'storax/elpy-test-tox-pytest-runner' with default values.
TOP is the project root.
FILE the test file.
MODULE is the module to test or nil to test all.
TEST is a single test function or nil to test all."
  (interactive (elpy-test-at-point))
  (let ((toxargs (car storax/tox-history))
	(pytestargs (car storax/pytest-history)))
  (storax/run-tox-pytest toxargs pytestargs top file module test)))

(put 'storax/elpy-test-tox-pytest-runner 'elpy-test-runner-p t)
(setq elpy-test-runner 'storax/elpy-test-tox-pytest-runner)



;;----------------------------------------------------------------------------
;; Guess initial value
;;----------------------------------------------------------------------------
(require 'pyvenv)
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

(require 'helm-regexp)
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
        :truncate-lines helm-moccur-truncate-lines))

(define-key elpy-mode-map (kbd "C-c C-o") 'python-helm-occur)

;;----------------------------------------------------------------------------
;; Key bindings
;;----------------------------------------------------------------------------
;;Python mode move around code blocks
(define-key python-mode-map (kbd "M-p") 'python-nav-backward-block)
(define-key python-mode-map (kbd "M-n") 'python-nav-forward-block)

(define-key python-mode-map (kbd "<tab>") 'storax/shift-or-indent)
(define-key python-mode-map (kbd "C-<tab>") 'storax/dabbrev-or-indent-left)
(define-key elpy-mode-map (kbd "C-c t") 'storax/elpy-test-tox-pytest-runner-default)

(require 'init-flycheck)
(define-key elpy-mode-map (kbd "C-c C-p") 'storax/previous-error-wrapped)
(define-key elpy-mode-map (kbd "C-c C-n") 'storax/next-error-wrapped)

;;----------------------------------------------------------------------------
;; Key bindings
;;----------------------------------------------------------------------------
(defalias 'workon 'pyvenv-workon)

;;----------------------------------------------------------------------------
;; Hooks
;;----------------------------------------------------------------------------
(add-hook 'python-mode-hook 'hs-minor-mode)
(add-hook 'python-mode-hook #'electric-operator-mode)

(provide 'init-elpy)
;;; init-elpy ends here
