;;; init-helm-dash --- Browse Docs

;;; Commentary:

;;; Code:
(require 'init-elpa)
(require-package 'helm-dash)
(require 'helm-dash)

(defvar storax/docsets-to-install '("Ansible"
				    "Bash"
				    "Boost"
				    "C"
				    "C++"
				    "CMake"
				    "Chef"
				    "Common_Lisp"
				    "Django"
				    "Docker"
				    "ElasticSearch"
				    "Emacs_Lisp"
				    "Flask"
				    "Jinja"
				    "LaTeX"
				    "PostgreSQL"
				    "Python_2"
				    "Python_3"
				    "SQLAlchemy"
				    "Vagrant"))

(defvar storax/user-docsets-to-install '("Alembic"
					 "Packer"
					 "PyMel"
					 "Requests"
					 "Sphinx"))


(defun storax/dash-install-set (docset &optional escaped)
  "Install the DOCSET if it does not exist already."
  (let ((name-escaped (or escaped (replace-regexp-in-string "_" " " docset))))
    (unless (file-exists-p
	     (expand-file-name (concat (file-name-as-directory ".docsets")
				       (concat name-escaped ".docset"))
			       "~"))
      (helm-dash-install-docset docset))))

(defun storax/dash-install-user-set (docset &optional escaped)
  "Install the user DOCSET if it does not exist already."
  (let ((name-escaped (or escaped (replace-regexp-in-string "_" " " docset))))
    (unless (file-exists-p
	     (expand-file-name (concat (file-name-as-directory ".docsets")
				       (concat name-escaped ".docset"))
			       "~"))
      (helm-dash-install-user-docset docset))))

(dolist (docset storax/docsets-to-install)
  (storax/dash-install-set docset))
(dolist (docset storax/user-docsets-to-install)
  (storax/dash-install-user-set docset))
(storax/dash-install-set "Qt_4" "Qt")
(storax/dash-install-user-set "Emacs" "emacs")

(setq helm-dash-common-docsets '("Bash"
				 "Common Lisp"
				 "Emacs Lisp"
				 "Packer"
				 "Python 2"
				 "Python 3"
				 "Qt"
				 "Requests"
				 "Sphinx"
				 "Vagrant"
				 "emacs"))

(provide 'init-helm-dash)
;;; init-helm-dash ends here
