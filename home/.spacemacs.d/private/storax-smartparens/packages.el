;;; packages.el --- storax-smartparens layer packages file for Spacemacs.
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

(defconst storax-smartparens-packages
  '(smartparens)
  "The list of Lisp packages required by the storax-smartparens layer.")

(defun storax-smartparens/post-init-smartparens ()
  (use-package smartparens
    :defer t
    :config
    (defun storax/swap-place-in-sexp ()
      "Go to the other end of the current sexpression."
      (interactive)
      (let ((next (sp-get-paired-expression))
            (previous (sp-get-paired-expression t)))
        (if (and next previous)
            (let* ((p (point))
                   (closest (storax/closer-pair next previous p))
                   (beg (plist-get closest ':beg))
                   (end (plist-get closest ':end)))
              (if (> (abs (- p beg)) (abs (- p end)))
                  (if (>= p end)
                      (goto-char beg)
                    (goto-char (+ beg 1)))
                (if (<= p beg)
                    (goto-char end)
                  (goto-char (- end 1))))))))

    (defun storax/swap-place-in-region ()
      "Go to the other end of the current region.

Set a mark before moving.
If a region is active this acts like swap places."
      (interactive)
      (if (region-active-p)
          (let ((next (if (equal (point) (region-beginning))
                          (region-end)
                        (region-beginning))))
            (set-mark-command nil)
            (goto-char next)
            (setq deactivate-mark nil))
        (storax/swap-place-in-sexp)))

    (bind-key "C-M-." 'storax/swap-place-in-region)
    (bind-key "C-M-," 'storax/swap-place-in-region)))

;;; packages.el ends here
