;;; funcs.el --- storax-lame layer functions file for Spacemacs.
;;
;; Copyright (c) 2016 David Zuber
;;
;; Author: David Zuber <zuber.david@gmx.de>
;; URL: https://github.com/storax/emacs-castle
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;;; Code:

(defvar lame-continue-callback nil
  "Callback function continue current lame session.")

(defvar lame-task nil
  "The current task.")

(defvar lame-task-buffer nil
  "The current task buffer.")

(defvar lame-edit-buffer nil
  "The current edit buffer.")

(defun lame/interactive-args (func)
  "Get the interactive args of FUNC."
  (advice-eval-interactive-spec (cadr (interactive-form func))))

(defmacro lame/let-interactive (spec &rest body)
  "Get the interactive args of FUNC and let-bind them to SYMBOLS.
Then evaluate BODY.
SYMBOLS should be a list the length of the interactive args of FUNC.

\(fn (FUNC SYMBOLS) BODY...)"
  (let* ((func (car spec))
         (symbols (cadr spec))
         (args (lame/interactive-args func))
         (letlist (mapcar* #'list (setcdr (last symbols) symbols) args))) ;zipd
         `(let ,letlist
            ,@body)))

(defmacro lame/task (taskname description &rest body)
  "Create a task with TASKNAME and DESCRIPTION and execute BODY.
The task is marked as finished after calling `lame/done'."
  `(let ((taskbuffername (format "Task: %s" ,taskname)))
     (when (or (not lame-task) (y-or-n-p "Cancel currently running task?"))
       (display-buffer (get-buffer-create taskbuffername) '(display-buffer-reuse-window))
       (setq lame-task ,taskname
             lame-task-buffer (get-buffer taskbuffername)
             lame-continue-callback nil)
       (with-current-buffer lame-task-buffer
         (setq buffer-read-only t)
         (let ((inhibit-read-only t))
           (org-mode)
           (goto-char (point-max))
           (org-insert-heading nil nil t)
           (insert (format "Task: %s\n%s" ,taskname ,description))))
       ,@body)))

(defmacro lame/step (stepname description &rest body)
  "A step with STEPNAME of a task.
Add DESCRIPTION to the `lame-task-buffer'.
Then execute BODY."
  `(progn
     (display-buffer lame-task-buffer '(display-buffer-reuse-window))
     (with-current-buffer lame-task-buffer
       (let ((inhibit-read-only t))
         (goto-char (point-max))
         (org-insert-heading nil nil t)
         (org-demote)
         (insert (format "Step: %s\n%s" ,stepname ,description))))
     ,@body))

(defun lame/continue ()
  "Continue the current lame session."
  (interactive)
  (when lame-continue-callback
    (funcall lame-continue-callback)))

(defun lame/done ()
  "Call when you are done with the current task."
  (message "Task %s completed!" lame-task)
  (setq lame-task nil
        lame-continue-callback nil
        lame-task-buffer nil))

(defmacro lame/defer (before after)
  "Evaluate BEFORE and return it's return value.
Sets `lame-continue-callback' to evaluate AFTER."
  `(progn
     (setq lame-continue-callback
           (lambda () ,after))
     ,before))

(defmacro lame/show-buffer (msg buffer &rest body)
  "Show MSG and show BUFFER.
BODY will get executed when calling `lame/continue'."
  `(lame/confirm
    ,msg
    (switch-to-buffer ,buffer)
     ,@body))

(defmacro lame/confirm (prompt before &rest after)
  "Ask the user to continue with PROMPT after executing BEFORE.
If the user agrees execute AFTER else set AFTER as `lame-continue-callback'."
  `(progn
     ,before
     (if (y-or-n-p ,prompt)
         (progn ,@after)
       (setq lame-continue-callback
             (lambda () ,@after)))))

(defmacro lame/switch-branch (branch repo &rest after)
  "Checkout BRANCH in a git REPO.
Execute AFTER afterwards."
  `(lame/confirm
    (format "Checkout branch %s now?" ,branch)
    (magit-status ,repo)
    (magit-checkout ,branch)
    ,@after))

(defmacro lame/edit-text (msg var text mode &rest after)
  "Show MSG and an edit buffer.
Once the user calls `lame-continue' VAR is set with the current text of the buffer.
TEXT is the initial text in the buffer.
Execute AFTER afterwards."
  `(lame/defer
    (progn
      (setq lame-edit-buffer (switch-to-buffer-other-window (format "EDIT_%s" ,var)))
      (erase-buffer)
      (insert ,text)
      (when ,mode
        (funcall ,mode))
      (message ,msg))
    (progn
      (set ,var (buffer-string))
      (kill-buffer-and-window)
      (setq lame-edit-buffer nil)
      ,@after)))

(defun lame/add-text-to-description (text &optional wrap lang)
  "Add TEXT to the description of the current task/step.
If WRAP is non-nil wrap the text in a #+BEGIN_<wrap> #+END_<wrap> block.
If LANG is non-nil add it like this:  #+BEGIN_<wrap> <lang>."
  (with-current-buffer lame-task-buffer
    (goto-char (point-max))
    (let* ((inhibit-read-only t))
      (if wrap
          (insert
           (format
            "\n#+BEGIN_%s%s\n%s\n#+END_%s"
            wrap (if lang (concat " " lang) "") text wrap))
        (insert (concat "\n" text))))))

(defun example-commit (version)
  "Example commitng with a releasemessage."
  (interactive "sVersion: ")
  (eval
    `(lame/task
      (format "Release %s" ,version)
      (format "Releasing version %s" ,version)
      (lame/step
       "Checkout master"
       "Checkout master branch."
       (lame/switch-branch
        "master" "~/projects/emaci/"
        (lame/step
         "Update README.rst"
         "Add release notes to readme."
         (lame/edit-text
          "Edit release notes!"
          'releasenotes
          (format "Releasenotes for version %s:\n%s" ,version (shell-command-to-string "git status"))
          nil
          (lame/add-text-to-description releasenotes "EXAMPLE" "python")
          (lame/step
           "Commit release notes"
           "Use release notes as commit message."
           (kill-new releasenotes)
           (let ((default-directory "~/projects/emaci/"))
             (magit-commit))
           (lame/done)))))))))

;;; funcs.el ends here
