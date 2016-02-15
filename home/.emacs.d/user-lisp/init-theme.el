;;; init-theme --- Set the theme

;;; Commentary:

;;; Code:
(require 'init-elpa)
(require-package 'zenburn-theme)

(if window-system
    (load-theme 'zenburn t)
    (load-theme 'wombat t))

(provide 'init-theme)
;;; init-theme ends here
