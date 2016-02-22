;;; funcs.el --- storax-org layer functions file for Spacemacs.
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

(require 'cl-lib)

(defun storax/org-buffers ()
  "Return a list of org buffers."
  (let (buffers)
    (dolist (b (buffer-list))
      (when (with-current-buffer b (equal major-mode 'org-mode))
        (add-to-list 'buffers (buffer-name b))))
    buffers))

(defun storax/org-source-file ()
  "Create a nice abbreviation for the current file."
  (let ((prjname (projectile-project-name))
        (fullfile (buffer-file-name)))
    (if (eq prjname "-")
        fullfile
      (format
       "%s:%s" prjname
       (substring
        fullfile (+ (cl-search prjname fullfile) (length prjname) 1)
        (length fullfile))))))
;;; funcs.el ends here
