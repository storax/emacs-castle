;;; packages.el --- storax-iedit layer packages file for Spacemacs.
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

(defconst storax-iedit-packages
  '(iedit))

(defun storax-iedit/post-init-iedit ()
  (use-package iedit
    :bind ("C-;" . iedit-mode)))


;;; packages.el ends here
