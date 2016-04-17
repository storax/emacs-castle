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
(require 'ansi-color)
(require 'comint)
(require 'gud)
(require 'python)
(eval-when-compile (require 'cl-lib))

(cl-defstruct storax-bp bpnumber type disp enabled file line hits condition ignore)

(defvar storax-pdb-print-hist nil "History for pdb print commands.")

(defun storax-strip-whitespace (string)
  "Return STRING stripped of all whitespace."
  (while (string-match "^[\r\n\t ]+" string)
    (setq string (replace-match "" t t string)))
  string)

(defun storax/tox-env-list ()
  "Get a list of tox environments"
  (let* ((default-directory (projectile-project-root))
         (versions (split-string (shell-command-to-string "tox -l"))))
    (append (list "*default*" "ALL") versions)))

(defun storax/tox-read-env ()
  "Read virtual environment from user input."
  (let ((envlist (storax/tox-env-list))
        (prompt "Tox env: "))
  (if (fboundp 'helm-comp-read)
      (helm-comp-read
       prompt envlist
       :buffer "tox environments"
       :must-match t
       :history storax/tox-env-hist
       :marked-candidates t)
    (completing-read-multiple prompt envlist nil t nil storax/tox-env-hist))))

(defun storax/tox-construct-env-arg (envs)
  "Construct the -e arg out of ENVS."
  (if (member "*default*" envs)
      ""
      (concat " -e " (mapconcat 'identity envs ","))))

(defun storax/set-flycheck-error-function ()
  (setq flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list))

(defun storax/tox (envs args)
  "Test with tox.

ARGS is a string with arguments for tox."
  (interactive (list
                (storax/tox-read-env)
                (read-string "Tox arguments: " (car storax/tox-history) 'storax/tox-history)))
  (projectile-with-default-dir (projectile-project-root)
    (compilation-start (format "tox%s %s" (storax/tox-construct-env-arg envs) args) t)))

(defun storax/run-tests (cmd)
  "Execute the given CMD via compilation-start."
  (compilation-start cmd t))

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
    (storax/run-tests (format "tox %s" toxargs)))))

(defun storax/run-tox-pytest (envs toxargs pytestargs top file module test)
  "Run tox with pytest.

ENVS list of tox environments.
TOXARGS are the arguments for tox.
PYTESTARGS are the arguments for pytest.
TOP is the project root.
FILE the test file.
MODULE is the module to test or nil to test all.
TEST is a single test function or nil to test all."
  (let ((envarg (storax/tox-construct-env-arg envs)))
    (projectile-with-default-dir (projectile-project-root)
      (cond
       (test
        (storax/run-tests (concat
                           (format "tox%s %s -- py.test %s -k \"%s\" %s "
                                   envarg toxargs pytestargs test file))))
       (module
        (storax/run-tests (format "tox%s %s -- py.test %s %s" envarg toxargs pytestargs file)))
       (t
        (storax/run-tests (format "tox%s %s -- py.test %s" envarg toxargs pytestargs)))))))

(defun storax/python-test-tox-pytest-runner (top file module test)
  "Test the project using tox and pytest.

This requires the tox package to be installed and pytest as test suite in tox.
TOP is the project root.
FILE the test file.
MODULE is the module to test or nil to test all.
TEST is a single test function or nil to test all."
  (interactive (storax/python-test-at-point))
  (let ((envs (storax/tox-read-env))
        (toxargs (read-string "Tox arguments: " (car storax/tox-history) 'storax/tox-history))
	(pytestargs (read-string "py.test arguments: " (car storax/pytest-history) 'storax/pytest-history)))
  (storax/run-tox-pytest envs toxargs pytestargs top file module test)))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PDB stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar storax-pdb-output-filter-in-progress nil)
(defvar storax-pdb-output-filter-buffer nil)
(defvar storax-pdb-input-regexp "^[(<]*[Ii]?[Pp]db[>)]+ ")

(defun storax/pdb-output-filter (string)
  "Filter used in `storax/pdb-call-no-output' to grab output.
STRING is the output received to this point from the process.
This filter saves received output from the process in
`storax-pdb-output-filter-buffer' and stops receiving it after
detecting a prompt at the end of the buffer."
  (setq
   string (ansi-color-filter-apply string)
   storax-pdb-output-filter-buffer
   (concat storax-pdb-output-filter-buffer string))
  (when (string-match
         ;; XXX: It seems on OSX an extra carriage return is attached
         ;; at the end of output, this handles that too.
         (concat
          "\r?\n"
          ;; Remove initial caret from calculated regexp
          (replace-regexp-in-string
           (rx string-start ?^) ""
           storax-pdb-input-regexp)
          "$")
         storax-pdb-output-filter-buffer)
    ;; Output ends when `storax-pdb-output-filter-buffer' contains
    ;; the prompt attached at the end of it.
    (setq storax-pdb-output-filter-in-progress nil
          storax-pdb-output-filter-buffer
          (substring storax-pdb-output-filter-buffer
                     0 (match-beginning 0))))
  (if storax-pdb-output-filter-in-progress
      ""
    "(Pdb) "))

(defun storax/pdb-call-no-output (string)
  "Send STRING to PROCESS and inhibit ouput.
Return the output."
  (let ((comint-preoutput-filter-functions
         '(storax/pdb-output-filter))
        (storax-pdb-output-filter-in-progress t)
        (inhibit-quit t))
    (gud-call string)
    (while storax-pdb-output-filter-in-progress
      ;; `storax/pdb-output-filter' takes care of setting
      ;; `storax/pdb--output-filter-in-progress' to NIL after it
      ;; detects end of output.
      (accept-process-output (get-buffer-process gud-comint-buffer)))
    (prog1
        storax-pdb-output-filter-buffer
      (setq storax-pdb-output-filter-buffer nil))))

(defun storax/pdb-parse-breakpoint (line)
  "Return a breakpoint parsed from LINE."
  (string-match
   "^\\([0-9]+\\)[ \\t]+\\(\\w+\\)[ \\t]+\\(\\w+\\)[ \\t]+\\(\\w+\\)[ \\t]+at[ \\t]+\\(.*\\):\\([0-9]+\\)"
   line)
  (make-storax-bp
   :bpnumber (string-to-number (match-string 1 line))
   :type (match-string 2 line)
   :disp (match-string 3 line)
   :enabled (string= "yes" (match-string 4 line))
   :file (match-string 5 line)
   :line (string-to-number (match-string 6 line))
   :hits 0
   :condition nil
   :ignore 0))

(defun storax/pdb-parse-condition (line)
  "Return the condition string from LINE."
  (string-match
   "^[ \t]+stop only if \\(.*\\)$" line)
  (match-string 1 line))

(defun storax/pdb-parse-ignore (line)
  "Return ignore from LINE."
  (string-match
   "^[ \t]+ignore next \\([0-9]+\\) hits?$" line)
  (string-to-number (match-string 1 line)))

(defun storax/pdb-parse-hits (line)
  "Return hits from LINE."
  (string-match
   "^[ \t]+breakpoint already hit \\([0-9]+\\) times?$" line)
  (string-to-number (match-string 1 line)))

(defun storax/pdb-get-breakpoints ()
  "Return a list of breakpoints."
  (let ((output
         (cdr (split-string
          (storax/pdb-call-no-output "break")
          "[\n\r]+")))
        breakpoints)
    (dolist (line output)
      (cond
       ((string-match "^[0-9]+" line)
        (add-to-list 'breakpoints (storax/pdb-parse-breakpoint line) t))
       ((string-match "^[ \t]+stop only if" line)
        (setf (storax-bp-condition (car (last breakpoints))) (storax/pdb-parse-condition line)))
       ((string-match "^[ \t]+ignore next" line)
        (setf (storax-bp-ignore (car (last breakpoints))) (storax/pdb-parse-ignore line)))
       ((string-match "^[ \t]+breakpoint already hit" line)
        (setf (storax-bp-hits (car (last breakpoints))) (storax/pdb-parse-hits line)))))
    breakpoints))

(defun storax/pdb-breakpoints-to-strings (breakpoints)
  "Convert the list of BREAKPOINTS to a list of strings."
  (let (strlist)
    (dolist (bp breakpoints)
      (let ((bpstr
             (format "%s: %s %s:%s"
                     (storax-bp-bpnumber bp)
                     (if (storax-bp-enabled bp)
                       "armed"
                       "off")
                     (storax-bp-file bp)
                     (storax-bp-line bp)))
            (condition (storax-bp-condition bp))
            (hits (storax-bp-hits bp))
            (ignore (storax-bp-ignore bp)))
        (when condition
          (setq bpstr (concat bpstr "\n  stop only if " condition)))
        (unless (zerop ignore)
          (setq bpstr (concat bpstr "\n  ignore next " (number-to-string ignore) " hit(s)")))
        (unless (zerop hits) 
          (setq bpstr (concat bpstr "\n  already hit " (number-to-string hits) " time(s)")))
        (add-to-list 'strlist bpstr t)))
    strlist))

(defun storax/pdb-print-symbol ()
  "Print the current symbol at point."
  (interactive)
  (gud-call (format "p %s" (symbol-at-point))))

(defun storax/pdb-pprint-symbol ()
  "Pretty print the current symbol at point."
  (interactive)
  (gud-call (format "pp %s" (symbol-at-point))))

(defun storax/pdb-print-line ()
  "Print the current line without preceding whitespace."
  (interactive)
  (gud-call
   (format "p %s"
           (storax-strip-whitespace
            (buffer-substring (line-beginning-position) (line-end-position))))))

(defun storax/pdb-pprint-line ()
  "Pretty print the current line without preceding whitespace."
  (interactive)
  (gud-call
   (format "pp %s"
           (storax-strip-whitespace
            (buffer-substring (line-beginning-position) (line-end-position))))))

(defun storax/pdb-execute-line ()
  "Execute the current line without preceding whitespace."
  (interactive)
  (gud-call
   (format "!%s"
           (storax-strip-whitespace
            (buffer-substring (line-beginning-position) (line-end-position))))))

(defun storax/pdb-print-region ()
  "Print the current region without preceding whitespace."
  (interactive)
  (if (region-active-p)
      (gud-call
       (format "p %s"
               (storax-strip-whitespace
                (buffer-substring (region-beginning) (region-end)))))
    (storax/pdb-print-line)))

(defun storax/pdb-pprint-region ()
  "Pretty print the current region without preceding whitespace."
  (interactive)
  (if (region-active-p)
      (gud-call
       (format "pp %s"
               (storax-strip-whitespace
                (buffer-substring (region-beginning) (region-end)))))
    (storax/pdb-print-line)))

(defun storax/pdb-execute-region ()
  "Execute the current region without preceding whitespace."
  (interactive)
  (if (region-active-p)
      (gud-call
       (format "!%s"
               (storax-strip-whitespace
                (buffer-substring (region-beginning) (region-end)))))
    (storax/pdb-execute-line)))

(defun storax/pdb-print-prompt ()
  "Prompt user what to print."
  (interactive)
  (let ((user-input
         (read-string "Print: " nil storax-pdb-print-hist (symbol-at-point))))
  (gud-call (format "p %s" user-input))))

(defun storax/pdb-pprint-prompt ()
  "Prompt user what to pretty print."
  (interactive)
  (let ((user-input
         (read-string "Pretty Print: " nil storax-pdb-print-hist (symbol-at-point))))
    (gud-call (format "pp %s" user-input))))

(defun storax/pdb-execute-prompt ()
  "Prompt user what to execute."
  (interactive)
  (let ((user-input
         (read-string "Execute: " nil storax-pdb-print-hist (symbol-at-point))))
    (gud-call (format "!%s" user-input))))

(defun storax/pdb (command-line)
  "Run pdb on program COMMAND-LINE in buffer `*gud-FILE*'.
The directory containing FILE becomes the initial working directory
and source-file directory for your debugger."
  (interactive
   (list (gud-query-cmdline 'pdb)))

  (gud-common-init command-line nil 'gud-pdb-marker-filter)
  (set (make-local-variable 'gud-minor-mode) 'pdb)

  (gud-def gud-break  "break %d%f:%l"  "\C-b" "Set breakpoint at current line.")
  (gud-def gud-remove "clear %d%f:%l"  "\C-d" "Remove breakpoint at current line")
  (gud-def gud-step   "step"         "\C-s" "Step one source line with display.")
  (gud-def gud-next   "next"         "\C-n" "Step one line (skip functions).")
  (gud-def gud-cont   "continue"     "\C-r" "Continue with display.")
  (gud-def gud-finish "return"       "\C-f" "Finish executing current function.")
  (gud-def gud-up     "up"           "<" "Up one stack frame.")
  (gud-def gud-down   "down"         ">" "Down one stack frame.")
  (gud-def gud-until   "until"         "\C-w" "Execute until higher line number.")
  (gud-def gud-print-prompt (storax/pdb-print-prompt) "\C-p" "Print prompt.")
  (gud-def gud-jump "jump %l"        "\C-j" "Set execution address to current line.")
  (gud-def gud-execute-prompt (storax/pdb-execute-prompt) "\C-e" "Execute Python statement.")
  ;; (setq comint-prompt-regexp "^(.*pdb[+]?) *")
  (setq comint-prompt-regexp storax-pdb-input-regexp)
  (setq paragraph-start comint-prompt-regexp)
  (run-hooks 'pdb-mode-hook))

;;; funcs.el ends here
