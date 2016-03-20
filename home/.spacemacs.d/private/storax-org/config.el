;;; config.el --- storax-org layer config file for Spacemacs.
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

(defvar storax/org-organization-task-id "74f0b137-8c5e-4486-ab25-509343ff2dc7"
  "Id of the default clocking task that has to live in some file.
Use `org-id-get-create' to create a new id for some task.")
(defvar storax/org-hide-scheduled-and-waiting-next-tasks t)
(defvar storax/org-source-link-file-hist nil
  "History for files to insert links in.")

(defvar storax-org-template-dir
  (mapconcat
   'file-name-as-directory
   (list (file-name-directory load-file-name) "templates")
   nil))

(defvar storax-org-rtd-theme-path
  (mapconcat
   'file-name-as-directory
   (list storax-org-template-dir "rtd")
   nil))
;;; config.el ends here
