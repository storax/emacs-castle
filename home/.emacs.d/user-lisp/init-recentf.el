;;; init-recentf --- Customize recent files

;;; Commentary:

;;; Code:
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-saved-items 1000
      recentf-exclude '("/tmp/" "/ssh:"))

(provide 'init-recentf)
;;; init-recentf ends here
