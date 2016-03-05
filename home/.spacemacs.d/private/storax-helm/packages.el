;;; packages.el --- storax-helm layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: David Zuber <zuber.david@gmx.de>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;;; Code:

(defconst storax-helm-packages
  '(helm helm-swoop helm-ag))

(defun storax-helm/post-init-helm ()
  (use-package helm
    :defer t
    :bind (("M-y" . helm-show-kill-ring)
           ("C-x C-f" . helm-find-files)
           ("C-x C-h C-i" . helm-imenu)
           ("C-x b" . helm-mini))
    :init
    (setq helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
          helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
          helm-ff-file-name-history-use-recentf t
          helm-kill-ring-threshold 2
          helm-candidate-number-limit 400)
    :config
    (bind-key "M-i" 'helm-swoop-from-isearch isearch-mode-map)
    (helm-adaptive-mode 1)
    (helm-push-mark-mode 1)
    (advice-add #'helm-preselect :around #'storax/helm-skip-dots)
    (advice-add #'helm-ff-move-to-first-real-candidate :around #'storax/helm-skip-dots)))

(defun storax-helm/post-init-helm-swoop ()
  (use-package helm-swoop
    :defer t
    :config
    (bind-key "C-r" 'helm-previous-line helm-swoop-map)
    (bind-key "C-s" 'helm-next-line helm-swoop-map)
    (bind-key "C-r" 'helm-previous-line helm-multi-swoop-map)
    (bind-key "C-s" 'helm-next-line helm-multi-swoop-map)))

(defun storax-helm/post-init-helm-ag ()
  (use-package helm-ag
    :defer t
    :config
    (defun helm-ag--do-ag-set-command ()
      (let ((cmd-opts (split-string helm-ag-base-command nil t)))
        (when helm-ag-command-option
          (setq cmd-opts (append cmd-opts (split-string helm-ag-command-option nil t))))
        (when helm-ag--extra-options
          (setq cmd-opts (append cmd-opts (split-string helm-ag--extra-options))))
        (when helm-ag-use-agignore
          (helm-aif (helm-ag--root-agignore)
              (setq cmd-opts (append cmd-opts (list "-p" it)))))
        (when helm-ag-ignore-patterns
          (setq cmd-opts
                (append cmd-opts
                        (mapcar 'helm-ag--construct-ignore-option
                                helm-ag-ignore-patterns))))
        (when helm-do-ag--extensions
          (setq cmd-opts (append cmd-opts (helm-ag--construct-extension-options))))
        (let (targets)
          (when helm-ag--buffer-search
            (setq targets (helm-ag--file-visited-buffers)))
          (setq helm-do-ag--commands
                (cons cmd-opts
                      (if helm-ag--default-target
                          (append targets (helm-ag--construct-targets helm-ag--default-target))
                        targets))))))))

;;; packages.el ends here
