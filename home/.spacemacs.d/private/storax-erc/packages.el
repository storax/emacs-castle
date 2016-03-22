;;; packages.el --- storax-erc layer packages file for Spacemacs.
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

(defconst storax-erc-packages
  '(erc))

(defun storax-erc/post-init-erc ()
  (use-package erc
    :init
    (spacemacs/set-leader-keys "aig" 'storax-erc/erc-gitter)
    :config
    (setq erc-server "irc.gitter.im"
          erc-modules
          '(autojoin
            button
            completion
            fill
            hl-nicks
            image
            irccontrols
            list
            log
            match
            menu
            move-to-prompt
            netsplit
            networks
            noncommands
            pcomplete
            readonly
            ring
            stamp
            track
            youtube)
          erc-autojoin-channels-alist
          '(("irc.gitter.im" . ("signalpiller"
                                "magit/magit"
                                "syl20bnr/spacemacs"))))))


;;; packages.el ends here
