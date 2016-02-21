;;; funcs.el --- storax-helm layer functions file for Spacemacs.
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

(defun storax/helm-skip-dots (old-func &rest args)
  "Skip . and .. initially in helm-find-files.  First call OLD-FUNC with ARGS."
  (apply old-func args)
  (let ((sel (helm-get-selection)))
    (if (and (stringp sel) (string-match "/\\.$" sel))
        (helm-next-line 2)))
  (let ((sel (helm-get-selection))) ; if we reached .. move back
    (if (and (stringp sel) (string-match "/\\.\\.$" sel))
        (helm-previous-line 1))))

;;; funcs.el ends here
