;;; packages.el --- storax-org layer packages file for Spacemacs.
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

(defconst storax-org-packages
  '(org orgbox))

(defun storax-org/init-orgbox ()
  (use-package orgbox))

(defun storax-org/init-org ()
  (use-package org
    :defer t
    :config
    (defun storax/org-insert-source-link ()
      "Create a source link to the current line in the file."
      (interactive)
      (let ((srcfile (storax/org-source-file))
            (fullfile (buffer-file-name))
            (lineno (line-number-at-pos
                     (if (region-active-p)
                         (region-beginning)
                       (point))))
            (lineendno (line-number-at-pos
                        (if (region-active-p)
                            (region-end)
                          (point))))
            (orgbufs (storax/org-buffers))
            selectedbuf
            linestr)
        (if (equal lineno lineendno)
            (setq linestr (format "l.%s" lineno))
          (setq linestr (format "l.%s-l.%s" lineno lineendno)))
        (unless orgbufs
          (add-to-list
           'orgbufs
           (get-buffer-create (read-from-minibuffer "No org buffer found. New buffer name: ")))
          (with-current-buffer (car orgbufs)
            (org-mode)))
        (if (> (length orgbufs) 1)
            (setq selectedbuf
                  (completing-read
                   "Choose buffer to insert link: "
                   orgbufs nil t nil
                   'storax/org-source-link-file-hist))
          (setq selectedbuf (car orgbufs)))
        (switch-to-buffer-other-window selectedbuf)
        (goto-char (point-max))
        (unless (eq (point) (line-beginning-position))
          (newline))
        (org-insert-heading)
        (insert (org-make-link-string
                 (format "file:%s::%s" fullfile lineno)
                 (format "%s:%s" srcfile linestr)))
        (insert "\n")))))


;;; packages.el ends here
