;;; init-expand-region --- Configure expand region

;;; Commentary:

;;; Code:
(require 'init-elpa)
(require-package 'expand-region)

;;----------------------------------------------------------------------------
;; Key Bindings
;;----------------------------------------------------------------------------

(global-set-key (kbd "C-.") 'er/expand-region)
(global-set-key (kbd "C-,") 'er/contract-region)

(provide 'init-expand-region)
;;; init-expand-region ends here
