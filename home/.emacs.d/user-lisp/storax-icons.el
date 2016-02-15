;;; storax-icons --- A set of icons

;;; Commentary:

;;; Code:

(require 'cl-lib)
(cl-defun storax/create-icon (path &optional (asc 80) (text "  "))
  "Return string with icon at PATH displayed with ascent ASC and TEXT."
  (propertize text 'display
	      (create-image
	       (with-temp-buffer
		 (insert-file-contents path) (buffer-string))
	       nil t :ascent asc :mask 'heuristic)))

(cl-defun storax/create-img (path &optional (text "  ") (asc 80))
  "Return string with icon at PATH displayed with ascent ASC and TEXT."
  (propertize text 'display (create-image path 'png nil :ascent asc :mask 'heuristic)))

(defconst storax/icon-file (storax/create-icon "~/.emacs.d/icons/file-file.svg"))
(defconst storax/icon-archive (storax/create-icon "~/.emacs.d/icons/file-archive.svg"))
(defconst storax/icon-config (storax/create-icon "~/.emacs.d/icons/file-config.svg"))
(defconst storax/icon-cpp (storax/create-icon "~/.emacs.d/icons/file-cpp.svg"))
(defconst storax/icon-image (storax/create-icon "~/.emacs.d/icons/file-image.svg"))
(defconst storax/icon-xml (storax/create-icon "~/.emacs.d/icons/file-xml.svg"))
(defconst storax/icon-shell (storax/create-icon "~/.emacs.d/icons/file-shell.svg"))
(defconst storax/icon-yaml (storax/create-icon "~/.emacs.d/icons/file-yaml.svg"))
(defconst storax/icon-movie (storax/create-icon "~/.emacs.d/icons/file-movie.svg"))
(defconst storax/icon-python (storax/create-icon "~/.emacs.d/icons/file-py.svg"))
(defconst storax/icon-folder (storax/create-icon "~/.emacs.d/icons/file-folder.svg"))
(defconst storax/icon-json (storax/create-icon "~/.emacs.d/icons/file-json.svg"))
(defconst storax/icon-rst (storax/create-icon "~/.emacs.d/icons/file-rst.svg"))
(defconst storax/icon-txt (storax/create-icon "~/.emacs.d/icons/file-txt.svg"))
(defconst storax/icon-pdf (storax/create-icon "~/.emacs.d/icons/file-pdf.svg"))
(defconst storax/icon-coffee (storax/create-icon "~/.emacs.d/icons/file-coffee.svg"))
(defconst storax/icon-qss (storax/create-icon "~/.emacs.d/icons/file-qss.svg"))
(defconst storax/icon-css (storax/create-icon "~/.emacs.d/icons/file-css.svg"))
(defconst storax/icon-js (storax/create-icon "~/.emacs.d/icons/file-js.svg"))
(defconst storax/icon-el (storax/create-icon "~/.emacs.d/icons/file-el.svg"))
(defconst storax/icon-cl (storax/create-icon "~/.emacs.d/icons/file-cl.svg"))
(defconst storax/icon-ruby (storax/create-icon "~/.emacs.d/icons/file-ruby.svg"))
(defconst storax/icon-md (storax/create-icon "~/.emacs.d/icons/file-md.svg"))
(defconst storax/icon-java (storax/create-icon "~/.emacs.d/icons/file-java.svg"))
(defconst storax/icon-help (storax/create-icon "~/.emacs.d/icons/file-help.svg"))
(defconst storax/icon-mail (storax/create-icon "~/.emacs.d/icons/file-mail.svg"))
(defconst storax/icon-git (storax/create-icon "~/.emacs.d/icons/file-git.svg"))
(defconst storax/icon-git-stash (storax/create-icon "~/.emacs.d/icons/file-git-stash.svg"))
(defconst storax/icon-git-merge (storax/create-icon "~/.emacs.d/icons/file-git-merge.svg"))
(defconst storax/icon-git-commit (storax/create-icon "~/.emacs.d/icons/file-git-commit.svg" 80 "   "))
(defconst storax/icon-diff (storax/create-icon "~/.emacs.d/icons/file-git-diff.svg"))
(defconst storax/icon-cherry (storax/create-icon "~/.emacs.d/icons/file-cherry.svg"))
(defconst storax/icon-wheel (storax/create-icon "~/.emacs.d/icons/file-wheel.svg"))
(defconst storax/icon-log (storax/create-icon "~/.emacs.d/icons/file-log.svg"))
(defconst storax/icon-db (storax/create-icon "~/.emacs.d/icons/file-db.svg"))
(defconst storax/icon-sql (storax/create-icon "~/.emacs.d/icons/file-sql.svg"))
(defconst storax/icon-mpc (storax/create-img "~/.emacs.d/icons/mpc-small.png"))

(provide 'storax-icons)
;;; storax-icons ends here
