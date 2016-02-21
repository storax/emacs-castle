;;; packages.el --- storax-expand-region layer packages file for Spacemacs.
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

(defconst storax-expand-region-packages
  '(expand-region))

(defun storax-expand-region/post-init-expand-region ()
    (use-package expand-region
      :defer t
      :bind (("C-." . er/expand-region)
             ("C-," . er/contract-region))))

;;; packages.el ends here
