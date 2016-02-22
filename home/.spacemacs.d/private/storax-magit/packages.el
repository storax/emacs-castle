;;; packages.el --- storax-magit layer packages file for Spacemacs.
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

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `storax-magit-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `storax-magit/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `storax-magit/pre-init-PACKAGE' and/or
;;   `storax-magit/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst storax-magit-packages
  '(magit))

(defun storax-magit/post-init-magit ()
  (use-package magit
    :bind ("C-x g" . magit-status)))


;;; packages.el ends here
