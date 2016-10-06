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
    :bind (("C-x C-f" . helm-find-files)
           ;("M-y" . helm-show-kill-ring)
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
    :init
    (progn
      (spacemacs/declare-prefix storax-helm-ag-dirs-prefix "dir" "helm-ag in dir")
      (storax/create-helm-ag-bindings storax-helm-ag-dirs-alist))))

;;; packages.el ends here
