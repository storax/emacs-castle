;;; init-diff-hl.el --- Display diffs in side column

;;; Commentary:

;;; Code:
(require-package 'fringe-helper)
(require 'fringe-helper)

(fringe-helper-define 'storax/asterisk-mark nil
  "...X...."
  "X..X..X."
  ".X.X.X.."
  "..XXX..."
  "..XXX..."
  ".X.X.X.."
  "X..X..X."
  "...X....")

(defun storax/diff-hl-fringe-bmp-from-type (type _pos)
  (cl-case type
    (unknown 'question-mark)
    (change 'storax/asterisk-mark)
    (ignored 'diff-hl-bmp-i)
    (t (intern (format "diff-hl-bmp-%s" type)))))

(defadvice diff-hl-mode (after storax/diff-hl-fix-magit activate)
  "Fix nesting error by removing the hook."
  (when diff-hl-mode
    (remove-hook 'magit-revert-buffer-hook 'diff-hl-update)))

(global-diff-hl-mode)
(diff-hl-flydiff-mode)

(provide 'init-diff-hl)
;;; init-diff-hl.el ends here
