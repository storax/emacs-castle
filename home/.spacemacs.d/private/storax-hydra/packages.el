;;; packages.el --- storax-hydra layer packages file for Spacemacs.
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

(defconst storax-hydra-packages
  '(hydra move-text)
  "The list of Lisp packages required by the storax-hydra layer.")


(defun storax-hydra/post-init-hydra ()
    (storax/define-hydras))

(defun storax/define-hydras ()
  "Define all the hydras."
  (require 'hydra-examples)
  (defhydra hydra-goto-line
    (goto-map "")
    ("g" goto-line "go")
    ("m" set-mark-command "mark" :bind nil)
    ("q" nil "quit"))

  (defhydra hydra-window-stuff
    (:hint nil)
    "
          Split: _v_ert  _s_:horz
         Delete: _c_lose  _o_nly
  Switch Window: _h_:left  _j_:down  _k_:up  _l_:right _SPC_:ace
        Buffers: _p_revious  _n_ext  _b_:select  _f_ind-file  _F_:projectile
         Winner: _u_ndo  _r_edo
         Resize: _H_:splitter left  _J_:splitter down  _K_:splitter up  _L_:splitter right
           Move: _a_:up  _z_:down  _i_:swoop"

    ("z" scroll-up-line)
    ("a" scroll-down-line)
    ("i" helm-swoop)

    ("u" winner-undo)
    ("r" winner-redo)

    ("h" windmove-left)
    ("j" windmove-down)
    ("k" windmove-up)
    ("l" windmove-right)
    ("SPC" ace-window)

    ("p" previous-buffer)
    ("n" next-buffer)
    ("b" ido-switch-buffer)
    ("f" helm-find-file)
    ("F" helm-projectile-find-file-dwim)

    ("s" split-window-below)
    ("v" split-window-right)

    ("c" delete-window)
    ("o" delete-other-windows)

    ("H" hydra-move-splitter-left)
    ("J" hydra-move-splitter-down)
    ("K" hydra-move-splitter-up)
    ("L" hydra-move-splitter-right)

    ("q" nil))
  (spacemacs/set-leader-keys
    "w SPC" 'hydra-window-stuff/body))

(defun storax-hydra/post-init-move-text ()
  (defhydra hydra-move-text ()
    "Move text"
    ("p" move-text-up "up")
    ("n" move-text-down "down")
    ("q" nil "quit"))

  (spacemacs/set-leader-keys
    "xp" 'hydra-move-text/move-text-up
    "xn" 'hydra-move-text/move-text-down))

;;; packages.el ends here
