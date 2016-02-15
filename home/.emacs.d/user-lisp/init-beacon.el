;;; init-beacon --- Flash Cursor

;;; Commentary:

;;; Code:
(require 'init-elpa)
(require-package 'beacon)

(require 'beacon)
(beacon-mode 1)
(setq beacon-size 30
      beacon-blink-delay 0.1
      beacon-blink-duration 0.2
      beacon-color "sandy brown")

(provide 'init-beacon)
;;; init-beacon ends here
