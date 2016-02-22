;;; funcs.el --- storax-python layer functions file for Spacemacs.
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

;; Credits to Jorgen Schaefer's elpy for the find test functions.

;;; Code:

(defun storax/set-flycheck-error-function ()
  (setq flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list))

(defun storax/tox (args)
  "Test with tox.

ARGS is a string with arguments for tox."
  (interactive (list (read-string "Tox arguments: " (car storax/tox-history) 'storax/tox-history)))
  (projectile-with-default-dir (projectile-project-root)
    (compilation-start (format "tox %s" args) t)))

(defun storax/python-test--current-test-name ()
  (let ((name (python-info-current-defun)))
    (if (and name
             (string-match "\\`\\([^.]+\\.[^.]+\\)\\." name))
        (match-string 1 name)
      name)))

(defun storax/python-test--module-name-for-file (top-level module-file)
  "Return the module name relative to TOP-LEVEL for MODULE-FILE.
For example, for a top level of /project/root/ and a module file
of /project/root/package/module.py, this would return
\"package.module\"."
  (let* ((relative-name (file-relative-name module-file top-level))
         (no-extension (replace-regexp-in-string "\\.py\\'" "" relative-name))
         (no-init (replace-regexp-in-string "/__init__\\'" "" no-extension))
         (dotted (replace-regexp-in-string "/" "." no-init)))
    (if (string-match "^\\." dotted)
        (concat "." (replace-regexp-in-string (regexp-quote "...") "." dotted))
      dotted)))

(defun storax/python-library-root ()
  "Return the root of the Python package chain of the current buffer.
That is, if you have /foo/package/module.py, it will return /foo,
so that import package.module will pick up module.py."
  (locate-dominating-file default-directory
                          (lambda (dir)
                            (not (file-exists-p
                                  (format "%s/__init__.py"
                                          dir))))))

(defun storax/python-test-at-point ()
  "Return a list specifying the test at point, if any.
This is used as the interactive
This list has four elements.
- Top level directory:
  All test files should be importable from here.
- Test file:
  The current file name.
- Test module:
  The module name, relative to the top level directory.
- Test name:
  The full name of the current test within the module, for
  example TestClass.test_method
If there is no test at point, test name is nil.
If the current buffer is not visiting a file, only the top level
directory is not nil."
  (if (not buffer-file-name)
      (progn
        (save-some-buffers)
        (list (storax/python-library-root) nil nil nil))
    (let* ((top (storax/python-library-root))
           (file buffer-file-name)
           (module (storax/python-test--module-name-for-file top file))
           (test (storax/python-test--current-test-name)))
      (if (and file (string-match "test" (or module test "")))
          (progn
            (save-buffer)
            (list top file module test))
        (save-some-buffers)
        (list top nil nil nil)))))

(defun storax/python-test-tox-runner (top file module test)
  "Test the project using tox.

This requires the tox package to be installed.
TOP is the project root.
FILE the test file.
MODULE is the module to test or nil to test all.
TEST is a single test function or nil to test all."
  (interactive (storax/python-test-at-point))
  (let (toxargs '(read-string "Tox arguments: " (car storax/tox-history) 'storax/tox-history))
  (projectile-with-default-dir (projectile-project-root)
    (compilation-start (format "tox %s" toxargs) t))))

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
	(compilation-start (concat
			      (format "tox %s -- py.test %s -k \"%s\" %s "
				      toxargs pytestargs test file)) t))
     (module
      (compilation-start (format "tox %s -- py.test %s %s" toxargs pytestargs file) t))
     (t
      (compilation-start (format "tox %s -- py.test %s" toxargs pytestargs) t)))))

(defun storax/python-test-tox-pytest-runner (top file module test)
  "Test the project using tox and pytest.

This requires the tox package to be installed and pytest as test suite in tox.
TOP is the project root.
FILE the test file.
MODULE is the module to test or nil to test all.
TEST is a single test function or nil to test all."
  (interactive (storax/python-test-at-point))
  (let ((toxargs (read-string "Tox arguments: " (car storax/tox-history) 'storax/tox-history))
	(pytestargs (read-string "py.test arguments: " (car storax/pytest-history) 'storax/pytest-history)))
  (storax/run-tox-pytest toxargs pytestargs top file module test)))

(defun storax/python-test-tox-pytest-runner-all (top file module test)
  "Test the project using tox and pytest.

This requires the tox package to be installed and pytest as test suite in tox.
TOP is the project root."
  (interactive (storax/python-test-at-point))
    (storax/python-test-tox-pytest-runner top nil nil nil))

(defun storax/python-test-tox-pytest-runner-module (top file module test)
  "Test the project using tox and pytest.

This requires the tox package to be installed and pytest as test suite in tox.
TOP is the project root.
FILE the test file.
MODULE is the module to test or nil to test all."
  (interactive (storax/python-test-at-point))
  (storax/python-test-tox-pytest-runner top file module nil))

(defun storax/python-test-tox-pytest-runner-module (top file module test)
  "Test the project using tox and pytest.

This requires the tox package to be installed and pytest as test suite in tox.
TOP is the project root."
  (interactive (storax/python-test-at-point))
  (storax/python-test-tox-pytest-runner top file module nil))

(defun storax/python-test-tox-pytest-runner-default (top file module test)
  "Test the project using tox and pytest.

This requires the tox package to be installed and pytest as test suite in tox.
Call `storax/python-test-tox-pytest-runner' with default values.
TOP is the project root.
FILE the test file.
MODULE is the module to test or nil to test all.
TEST is a single test function or nil to test all."
  (interactive (storax/python-test-at-point))
  (let ((toxargs (car storax/tox-history))
	(pytestargs (car storax/pytest-history)))
  (storax/run-tox-pytest toxargs pytestargs top file module test)))

;;; funcs.el ends here
