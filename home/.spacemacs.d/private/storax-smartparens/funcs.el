;;; funcs.el --- storax-smartparens layer functions file for Spacemacs.
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

(defun storax/min-offset (pair p)
  "Get minimum offset to either beginning or end or PAIR from P."
  (min (abs (- p (plist-get pair :beg))) (abs (- p (plist-get pair :end)))))

(defun storax/closer-pair (pair1 pair2 p)
  "Return one of PAIR1 or PAIR2 which is closer to P."
  (let ((off1 (storax/min-offset pair1 p))
        (off2 (storax/min-offset pair2 p)))
    (if (<= off1 off2)
        pair1
      pair2)))

;;; funcs.el ends here
