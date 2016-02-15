;;; init-elpa --- Load packages on demmand

;;; Commentary:

;;; Code:
(require 'package)

;;----------------------------------------------------------------------------
;; Standard package repositories
;;----------------------------------------------------------------------------
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives
             '("org" . "http://orgmode.org/elpa/"))
;;----------------------------------------------------------------------------
;; On-demand installation of packages
;;----------------------------------------------------------------------------

(defun require-package (package &optional min-version no-refresh)
  "Install given PACKAGE, optionally requiring MIN-VERSION.
If NO-REFRESH is non-nil, the available package lists will not be
re-downloaded in order to locate PACKAGE."
  (if (package-installed-p package min-version)
      t
    (if (or (assoc package package-archive-contents) no-refresh)
          (package-install package)
      (progn
        (package-refresh-contents)
        (require-package package min-version t)))))


(defun maybe-require-package (package &optional min-version no-refresh)
  "Try to install PACKAGE, and return non-nil if successful.
In the event of failure, return nil and print a warning message.
Optionally require MIN-VERSION.  If NO-REFRESH is non-nil, the
available package lists will not be re-downloaded in order to
locate PACKAGE."
  (condition-case err
      (require-package package min-version no-refresh)
    (error
     (message "Couldn't install package `%s': %S" package err)
     nil)))


;; Fire up package.el
(setq package-enable-at-startup nil)
(package-initialize)

(provide 'init-elpa)
;;; init-elpa ends here
