;;; init-avy --- The New Ace Jump

;;; Commentary:

;;; Code:
(require 'init-elpa)
(require-package 'avy)
(require 'avy)
(define-key global-map (kbd "C-c SPC") 'avy-goto-word-1)
(define-key global-map (kbd "C-c c SPC") 'avy-goto-char)
(define-key global-map (kbd "C-c c c SPC") 'avy-goto-line)

;;----------------------------------------------------------------------------
;; Use the same face for the nth key to press
;; If you have paths with different length, the
;; faces are selected according to the order you have to press them
;; This way the first key to press is always the same color and easier to
;; identify.
;; See `(face ,(nth (- len i) avy-lead-faces))
;; Note the (- len i)
;;----------------------------------------------------------------------------

(defun avy--overlay-at-full (path leaf)
  "Create an overlay with PATH at LEAF.
PATH is a list of keys from tree root to LEAF.
LEAF is normally ((BEG . END) . WND)."
  (let* ((path (mapcar #'avy--key-to-char path))
	 (str (propertize
	       (apply #'string (reverse path))
	       'face 'avy-lead-face))
	 (len (length path))
	 (beg (avy-candidate-beg leaf))
	 (wnd (cdr leaf))
	 end)
    (dotimes (i len)
      (set-text-properties (- len i 1) (- len i)
			   `(face ,(nth (- len i) avy-lead-faces))
			   str))
    (when (eq avy-style 'de-bruijn)
      (setq str (concat
		 (propertize avy-current-path
			     'face 'avy-lead-face-1)
		 str))
      (setq len (length str)))
    (with-selected-window wnd
      (save-excursion
	(goto-char beg)
	(let* ((lep (if (bound-and-true-p visual-line-mode)
			(save-excursion
			  (end-of-visual-line)
			  (point))
		      (line-end-position)))
	       (len-and-str (avy--update-offset-and-str len str lep)))
	  (setq len (car len-and-str))
	  (setq str (cdr len-and-str))
	  (setq end (if (= beg lep)
			(1+ beg)
		      (min (+ beg
			      (if (eq (char-after) ?\t)
				  1
				len))
			   lep)))
	  (when (and (bound-and-true-p visual-line-mode)
		     (> len (- end beg)))
	    (setq len (- end beg))
	    (let ((old-str (apply #'string (reverse path))))
	      (setq str
		    (substring
		     (propertize
		      old-str
		      'face
		      (if (= (length old-str) 1)
			  'avy-lead-face
			'avy-lead-face-0))
		     0 len)))))))
    (avy--overlay
     str beg end wnd
     (lambda (str old-str)
       (cond ((string= old-str "\n")
	      (concat str "\n"))
	     ((string= old-str "\t")
	      (concat str (make-string (max (- tab-width len) 0) ?\ )))
	     (t
	      ;; add padding for wide-width character
	      (if (eq (string-width old-str) 2)
		  (concat str " ")
		str)))))))

(provide 'init-avy)
;;; init-avy ends here
