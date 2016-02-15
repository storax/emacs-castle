;;; init-windmove --- Move between windows

;;; Commentary:

;; Arrow keys? Srsly? We need something else!

;;; Code:
(defun ignore-error-wrapper (fn)
  "Return new function to ignore errors of FN.

The function wraps a function with `ignore-errors' macro."
  (let ((fn fn))
    `(lambda ()
      (interactive)
      (ignore-errors
	(funcall ',fn)))))

(global-set-key (kbd "S-<left>") (ignore-error-wrapper 'windmove-left))
(global-set-key (kbd "S-<right>") (ignore-error-wrapper 'windmove-right))
(global-set-key (kbd "S-<up>") (ignore-error-wrapper 'windmove-up))
(global-set-key (kbd "S-<down>") (ignore-error-wrapper 'windmove-down))

(provide 'init-windmove)
;;; init-windmove ends here
