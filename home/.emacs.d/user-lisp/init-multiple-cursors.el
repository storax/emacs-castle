;;; init-multiple-cursors -- Configure Multiple Cursors

;;; Commentary:

;; Cool stuff but a little bit much for the brain

;;; Code:
(require 'init-elpa)
(require-package 'multiple-cursors)
(require 'multiple-cursors)

(global-set-key (kbd "C-c m") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-c C->") 'mc/skip-to-next-like-this)
(global-set-key (kbd "C-M->") 'mc/unmark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/skip-to-previous-like-this)
(global-set-key (kbd "C-M-<") 'mc/unmark-previous-like-this)
(global-set-key (kbd "C-c C-c C->") 'mc/mark-all-like-this)

(provide 'init-multiple-cursors)
;;; init-multiple-cursors ends here
