;;; init-drag-stuff -- Configure drag stuff

;;; Commentary:

;; Use M-up and M-down to move line or region

;;; Code:
(require 'init-elpa)
(require-package 'drag-stuff)

(require 'drag-stuff)
(drag-stuff-global-mode t)

(provide 'init-drag-stuff)
;;; init-drag-stuff ends here
