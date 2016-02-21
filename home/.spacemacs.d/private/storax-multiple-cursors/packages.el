;;; packages.el --- storax-multiple-cursors layer packages file for Spacemacs.
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

(defconst storax-multiple-cursors-packages
  '(multiple-cursors))

(defun storax-multiple-cursors/init-multiple-cursors ()
    (use-package multiple-cursors
      :defer t
      :bind (("C->" . mc/mark-next-like-this)
             ("C-<" . mc/mark-previous-like-this)
             ("C-c C->" . mc/skip-to-next-like-this)
             ("C-c C-<" . mc/skip-to-previous-like-this)
             ("C-M->" . mc/unmark-next-like-this)
             ("C-M-<" . mc/unmark-previous-like-this)
             ("C-c C-c C->" . mc/mark-all-like-this))
      :init
      (spacemacs/set-leader-keys "eml" 'mc/edit-lines)
      (spacemacs/set-leader-keys "ema" 'mc/mark-all-like-this)))


;;; packages.el ends here
