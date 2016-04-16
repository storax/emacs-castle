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

(defvar storax-pdb-print-hist nil "History for pdb print commands.")
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
  "dps" 'storax/gud-print-symbol
  "dpl" 'storax/gud-print-line
  "dpr" 'storax/gud-print-region
  "dpp SPC" 'storax/gud-pprint-prompt
  "dpps" 'storax/gud-pprint-symbol
  "dppl" 'storax/gud-pprint-line
  "dppr" 'storax/gud-pprint-region
  "dp SPC" 'storax/gud-print-prompt
  "del" 'storax/gud-execute-line
  "der" 'storax/gud-execute-region
  "de SPC" 'storax/gud-execute-prompt)

;;; config.el ends here
