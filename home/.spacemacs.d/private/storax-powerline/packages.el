;;; packages.el --- storax-powerline layer packages file for Spacemacs.
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

(defconst storax-powerline-packages
  '(spaceline))

(defun storax-powerline/post-init-spaceline ()
  (use-package spaceline-config
    :config
    nil))

;;; packages.el ends here
