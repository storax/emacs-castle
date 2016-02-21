;;; packages.el --- storax-ace-window layer packages file for Spacemacs.
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

(defconst storax-ace-window-packages
  '(ace-window))

(defun storax-ace-window/post-init-ace-window ()
  (use-package ace-window
    :bind ("C-z" . ace-window)))

;;; packages.el ends here
