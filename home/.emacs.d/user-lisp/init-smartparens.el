;;; init-smartparens -- Configure smartparens

;;; Commentary:

;;; Code:
(require 'init-elpa)
(require-package 'smartparens)
(require 'smartparens)

(smartparens-global-mode 1)
(show-smartparens-global-mode 1)

;;----------------------------------------------------------------------------
;; Custom Functions
;;----------------------------------------------------------------------------

(defun storax/swap-place-in-region ()
  "Go to the other end of the current region.

Set a mark before moving.
If a region is active this acts like swap places."
  (interactive)
  (if (region-active-p)
      (let ((next (if (equal (point) (region-beginning))
		      (region-end)
		    (region-beginning))))
	(set-mark-command nil)
	(goto-char next)
	(setq deactivate-mark nil))
    (storax/swap-place-in-sexp)))

(defun storax/min-offset (pair p)
  "Get minimum offset to either beginning or end or PAIR from P."
  (min (abs (- p (plist-get pair 'beg))) (abs (- p (plist-get pair 'end)))))

(defun storax/closer-pair (pair1 pair2 p)
  "Return one of PAIR1 or PAIR2 which is closer to P."
  (let ((off1 (storax/min-offset pair1 p))
	(off2 (storax/min-offset pair2 p)))
    (if (<= off1 off2)
	pair1
      pair2)))

(defun storax/swap-place-in-sexp ()
  "Go to the other end of the current sexpression."
  (interactive)
  (let ((next (sp-get-paired-expression))
	 (previous (sp-get-paired-expression t)))
    (if (and next previous)
	(let* ((p (point))
	 (closest (storax/closer-pair next previous p))
	 (beg (plist-get closest 'beg))
	 (end (plist-get closest 'end)))
	  (if (> (abs (- p beg)) (abs (- p end)))
	      (if (>= p end)
		  (goto-char beg)
		(goto-char (+ beg 1)))
	    (if (<= p beg)
		(goto-char end)
	      (goto-char (- end 1))))))))

(global-set-key (kbd "C-M-.") 'storax/swap-place-in-region)
(global-set-key (kbd "C-M-,") 'storax/swap-place-in-region)

(provide 'init-smartparens)
;;; init-smartparens ends here
