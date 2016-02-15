;;; init-helm --- Configure Helm

;;; Commentary:

;; We use the git version because it is more uptodate
;; Async is recommended. Dunno why. Works also without
;;

;;; Code:
(require 'init-elpa)
(require-package 'helm)
(require-package 'helm-swoop)
(require 'helm-config)
(require 'helm-ring)
(require 'helm-icons)
;;----------------------------------------------------------------------------
;; Helm Variables
;;----------------------------------------------------------------------------

(when (executable-find "curl")
  (setq helm-net-prefer-curl t))

(when (executable-find "ack-grep")
  (setq helm-grep-default-command "ack-grep -Hn --no-group --no-color %e %p %f"
	helm-grep-default-recurse-command "ack-grep -H --no-group --no-color %e %p %f"))

(setq helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
      helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-ff-file-name-history-use-recentf t
      helm-kill-ring-threshold 1
      helm-candidate-number-limit 400
)

;;----------------------------------------------------------------------------
;; Helm activate modes
;;----------------------------------------------------------------------------
(helm-mode 1)
(helm-adaptive-mode 1)
(helm-push-mark-mode 1)

;;----------------------------------------------------------------------------
;; Key Bindings
;;----------------------------------------------------------------------------
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x C-h C-i") 'helm-imenu)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-h SPC") 'helm-all-mark-rings)
(global-set-key (kbd "C-c h g") 'helm-google-suggest)

;;----------------------------------------------------------------------------
;; Helm-Swoop
;;----------------------------------------------------------------------------
(require 'helm-swoop)
(global-set-key (kbd "M-i") 'helm-swoop)
(global-set-key (kbd "M-I") 'helm-swoop-back-to-last-point)
(global-set-key (kbd "C-c M-i") 'helm-multi-swoop)
(global-set-key (kbd "C-x M-i") 'helm-multi-swoop-all)

;; When doing isearch, hand the word over to helm-swoop
(define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)
;; From helm-swoop to helm-multi-swoop-all
(define-key helm-swoop-map (kbd "M-i") 'helm-multi-swoop-all-from-helm-swoop)

;; Move up and down like isearch
(define-key helm-swoop-map (kbd "C-r") 'helm-previous-line)
(define-key helm-swoop-map (kbd "C-s") 'helm-next-line)
(define-key helm-multi-swoop-map (kbd "C-r") 'helm-previous-line)
(define-key helm-multi-swoop-map (kbd "C-s") 'helm-next-line)

;;----------------------------------------------------------------------------
;; Skip the first . and .. in helm-find-files
;;----------------------------------------------------------------------------

(defun storax/helm-skip-dots (old-func &rest args)
  "Skip . and .. initially in helm-find-files.  First call OLD-FUNC with ARGS."
  (apply old-func args)
  (let ((sel (helm-get-selection)))
    (if (and (stringp sel) (string-match "/\\.$" sel))
	(helm-next-line 2)))
  (let ((sel (helm-get-selection))) ; if we reached .. move back
    (if (and (stringp sel) (string-match "/\\.\\.$" sel))
	(helm-previous-line 1))))

(advice-add #'helm-preselect :around #'storax/helm-skip-dots)
(advice-add #'helm-ff-move-to-first-real-candidate :around #'storax/helm-skip-dots)

(provide 'init-helm)
;;; init-helm ends here
