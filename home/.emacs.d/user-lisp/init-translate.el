;;; init-translate --- Translate text

;;; Commentary:

;;; Code:
(require 'init-elpa)
(require-package 'google-translate)
(require 'google-translate)
(require 'google-translate-smooth-ui)

(setq google-translate-translation-directions-alist
      '(("en" . "de") ("de" . "en")))

(global-set-key (kbd "C-c d") 'google-translate-smooth-translate)
(provide 'init-translate)
;;; init-translate ends here
