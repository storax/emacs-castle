;;; config.el --- storax-compilation layer config file for Spacemacs.
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

(use-package diminish
  :defer t
  :init
  (with-eval-after-load 'drag-stuff
    (diminish 'compilation-shell-minor-mode)))

;;; config.el ends here
