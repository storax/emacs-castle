;;; packages.el --- storax-desktop layer packages file for Spacemacs.
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

(defconst storax-desktop-packages
  '(desktop))

(defun storax-desktop/post-init-desktop ()
  (use-package desktop
    :config
    (desktop-save-mode 1)
    (add-hook 'auto-save-hook 'storax/desktop-save)))

;;; packages.el ends here
