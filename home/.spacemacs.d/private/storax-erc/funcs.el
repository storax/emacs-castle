;;; funcs.el --- storax-powerline layer functions file for Spacemacs.
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

(configuration-layer/declare-layer 'storax-secret)

(defun storax-erc/erc-gitter ()
  (interactive)
  (let ((erc-server-connect-function 'erc-open-tls-stream))
    (storax/load-secrets)
    (erc :server "irc.gitter.im" :port "6667" :nick "storax" :password gitter-token)))

;;; funcs.el ends here
