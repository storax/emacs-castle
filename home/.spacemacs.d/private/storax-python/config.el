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
(defvar storax/pytest-history (list "-vv"))
(spacemacs/set-leader-keys-for-major-mode 'python-mode
  "tt" 'storax/python-test-tox-pytest-runner
  "ta" 'storax/python-test-tox-pytest-runner-all
  "tb" 'storax/python-test-tox-pytest-runner-module
  "tm" 'storax/python-test-tox-pytest-runner-module)

;;; config.el ends here
