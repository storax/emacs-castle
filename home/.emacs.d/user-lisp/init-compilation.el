;;; init-compilation --- Show nice ansi colors

;;; Commentary:

;;; Code:
(require 'compile)
(require 'ansi-color)

(defun storax/colorize-compilation-buffer ()
  "Colorize compilation buffer with ansi colors."
  (read-only-mode)
  (ansi-color-apply-on-region compilation-filter-start (point))
  (read-only-mode))

(add-hook 'compilation-filter-hook 'storax/colorize-compilation-buffer)

(provide 'init-compilation)
;;; init-compilation ends here
