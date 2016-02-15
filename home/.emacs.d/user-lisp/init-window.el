;;; init-window --- Customize the emacs window

;;; Commentary:

;;; Code:

;; No spashscreen, scratch message and default python mode
(setq inhibit-splash-screen t
      initial-scratch-message nil)

;; Hide tool and menu bar
(when window-system
  (tool-bar-mode -1)
  (menu-bar-mode -1))

;; Show little dashes to indicate empy lines
(setq-default indicate-empty-lines t)
(when (not indicate-empty-lines)
  (toggle-indicate-empty-lines))

(provide 'init-window)
;;; init-window ends here
