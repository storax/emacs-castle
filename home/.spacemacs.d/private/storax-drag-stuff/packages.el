;;; packages.el --- storax-drag-stuff layer packages file for Spacemacs.
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

(defconst storax-drag-stuff-packages
  '(drag-stuff))

(defun storax-drag-stuff/init-drag-stuff ()
  (use-package drag-stuff
    :config
    (drag-stuff-global-mode t)))

;; (defun storax-drag-stuff/post-init-diminish ()
;;   (use-package diminish
;;     :init
;;       (with-eval-after-load 'drag-stuff
;;        (diminish 'drag-stuff-mode))))

;;; packages.el ends here
