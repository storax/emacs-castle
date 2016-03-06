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

(require 'cl-lib)

(cl-defstruct storax-ci/job buildno status statusmsg buffer dir command mode highlight-regexp)

(defvar storax-ci/queue nil)
(defvar storax-ci/history nil)
(defvar storax-ci/buffer-job-alist nil)
(defvar storax-ci/build-counter 0)

(defun storax-ci/get-buildno ()
  "Get new build number and increase the counter `storax-ci/build-counter'."
  (setq storax-ci/build-counter (+ 1 storax-ci/build-counter)))

(defun storax-ci/new-job (dir command mode highlight-regexp)
  "Create a new job which gets executed in DIR.

Run compilation command COMMAND (low level interface).
If COMMAND starts with a cd command, that becomes the `default-directory'.
The rest of the arguments are optional; for them, nil means use the default.

MODE is the major mode to set in the compilation buffer.  Mode
may also be t meaning use function `compilation-shell-minor-mode'
under `comint-mode'.

If HIGHLIGHT-REGEXP is non-nil, `next-error' will temporarily highlight
the matching section of the visited source line; the default is to use the
global value of `compilation-highlight-regexp'."
  (let ((buildno (storax-ci/get-buildno)))
    (make-storax-ci/job
     :buildno buildno
     :status 'queued
     :statusmsg nil
     :buffer nil
     :dir dir
     :command command
     :mode mode
     :highlight-regexp highlight-regexp)))

(defun storax-ci/running-job-p ()
  "Return t if there is a running job."
  (let ((job (car storax-ci/queue)))
    (and job (eq (storax-ci/job-status job) 'running))))

(defun storax-ci/schedule (dir command &optional mode highlight-regexp)
  "Create and schedule a new job.

The job will get executed in DIR.

Run compilation command COMMAND (low level interface).
If COMMAND starts with a cd command, that becomes the `default-directory'.
The rest of the arguments are optional; for them, nil means use the default.

MODE is the major mode to set in the compilation buffer.  Mode
may also be t meaning use function `compilation-shell-minor-mode'
under `comint-mode'.

If HIGHLIGHT-REGEXP is non-nil, `next-error' will temporarily highlight
the matching section of the visited source line; the default is to use the
global value of `compilation-highlight-regexp'."
  (let ((job (storax-ci/new-job dir command mode highlight-regexp)))
    (add-to-list 'storax-ci/queue job t)
    (unless (storax-ci/running-job-p)
      (storax-ci/execute-next))))

(defun storax-ci/compilation-finished (buffer msg)
  "Callback when compilation buffer finishes in BUFFER with MSG.

Calls `storax-ci/job-finished'."
  (let ((job (cdr (assoc buffer storax-ci/buffer-job-alist))))
    (when job
      (storax-ci/job-finished job 'finished msg))))

(defun storax-ci/job-finished (job status statusmsg)
  "Callback when JOB finished with STATUS and STATUSMSG and execute the next."
  (message "Job finished %s: %s; %s" job status statusmsg)
  (setf (storax-ci/job-status job) status)
  (setf (storax-ci/job-statusmsg job) statusmsg)
  (add-to-list 'storax-ci/history job t)
  (setq storax-ci/queue (delete job storax-ci/queue))
  (storax-ci/execute-next))

(defun storax-ci/execute-next ()
  "Execute the next job in the queue."
  (let ((job (car storax-ci/queue)))
    (when job (storax-ci/execute job))))

(defun storax-ci/create-buffer-name (job)
  "Return a buffer name for JOB."
  (format "Build #%s" (storax-ci/job-buildno job)))

(defun storax-ci/create-buffer (job)
  "Create a buffer name for JOB."
  (let ((buffer (get-buffer-create (storax-ci/create-buffer-name job))))
    (add-to-list 'storax-ci/buffer-job-alist (cons buffer job))
    buffer))

(defun storax-ci/execute (job)
  "Execute the next JOB."
  (message "Execute: %s" job)
  (setf (storax-ci/job-status job) 'running)
  (setf (storax-ci/job-buffer job) (storax-ci/create-buffer job))
  (let ((default-directory (storax-ci/job-dir job)))
    (compilation-start
     (storax-ci/job-command job)
     (storax-ci/job-mode job)
     `(lambda (mode) (storax-ci/create-buffer-name ,job))
     (storax-ci/job-highlight-regexp job))))

(add-hook 'compilation-finish-functions 'storax-ci/compilation-finished)

;;; funcs.el ends here
