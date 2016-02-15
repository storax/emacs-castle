;;; org-source-link --- Create links to source files

;;; Commentary:

;; `storax/org-insert-source-link' will create a link heading at the end of
;; an org-mode buffer with the file name and line-no
;; Useful when reviewing lots of code and creating a list with comments.

;;; Code:
(require 'init-elpa)
(require-package 'org)
(require-package 'projectile)
(require 'projectile)
(require 'org)
(require 'cl-lib)

(defvar storax/org-source-link-file-hist nil
  "History for files to insert links in.")

(defun storax/org-buffers ()
  "Return a list of org buffers."
  (let (buffers)
  (dolist (b (buffer-list))
    (when (with-current-buffer b (equal major-mode 'org-mode))
      (add-to-list 'buffers (buffer-name b))))
  buffers))

(defun storax/org-source-file ()
  "Create a nice abbreviation for the current file."
  (let ((prjname (projectile-project-name))
	(fullfile (buffer-file-name)))
    (if (eq prjname "-")
        fullfile
      (format
       "%s:%s" prjname
       (substring
	fullfile (+ (cl-search prjname fullfile) (length prjname) 1)
	(length fullfile))))))

(defun storax/org-insert-source-link ()
  "Create a source link to the current line in the file."
  (interactive)
  (let ((srcfile (storax/org-source-file))
	(fullfile (buffer-file-name))
	(lineno (line-number-at-pos
		 (if (region-active-p)
		     (region-beginning)
		   (point))))
	(lineendno (line-number-at-pos
		    (if (region-active-p)
		     (region-end)
		     (point))))
	(orgbufs (storax/org-buffers))
	selectedbuf
	linestr)
    (if (equal lineno lineendno)
	(setq linestr (format "l.%s" lineno))
      (setq linestr (format "l.%s-l.%s" lineno lineendno)))
    (unless orgbufs
      (add-to-list
       'orgbufs
       (get-buffer-create (read-from-minibuffer "No org buffer found. New buffer name: ")))
      (with-current-buffer (car orgbufs)
	(org-mode)))
    (if (> (length orgbufs) 1)
	(setq selectedbuf
	      (completing-read
	       "Choose buffer to insert link: "
	       orgbufs nil t nil
	       'storax/org-source-link-file-hist))
      (setq selectedbuf (car orgbufs)))
    (switch-to-buffer-other-window selectedbuf)
      (goto-char (point-max))
      (unless (eq (point) (line-beginning-position))
	(newline))
      (org-insert-heading)
      (insert (org-make-link-string
	       (format "file:%s::%s" fullfile lineno)
	       (format "%s:%s" srcfile linestr)))
      (insert "\n")))

(provide 'org-source-link)
;;; org-source-link ends here
