;;; config.el --- storax-python layer config file for Spacemacs.
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

(defvar storax/tox-history (list) "History of tox arguments.")
(defvar storax/tox-env-hist nil)
(defvar storax/pytest-history (list "-vv"))
(spacemacs/set-leader-keys-for-major-mode 'python-mode
  "tt" 'storax/python-test-tox-pytest-runner
  "ta" 'storax/python-test-tox-pytest-runner-all
  "tb" 'storax/python-test-tox-pytest-runner-module
  "tm" 'storax/python-test-tox-pytest-runner-module
  "d SPC" 'gud-break
  "dj" 'gud-jump
  "dr" 'gud-remove
  "ds" 'gud-step
  "dn" 'gud-next
  "dc" 'gud-cont
  "du" 'gud-up
  "dd" 'gud-down
  "dU" 'gud-until
  "df" 'gud-finish
  "dg" 'storax/pdb-goto-breakpoint
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
  "de SPC" 'storax/pdb-execute-prompt)

;;; config.el ends here
