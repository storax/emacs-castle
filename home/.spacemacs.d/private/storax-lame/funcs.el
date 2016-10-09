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
  (declare (indent 1))
  (let* ((func (car spec))
         (symbols (cadr spec))
         (args (lame/interactive-args func))
         (letlist (mapcar* #'list (setcdr (last symbols) symbols) args))) ;zipd
         `(let ,letlist
            ,@body)))

(defmacro lame/task (taskname description &rest body)
  "Create a task called TASKNAME.

Create a task buffer to track the progress.
The initial DESCRIPTION is inserted.
Execute BODY as the task.
The task is marked as finished after calling `lame/done'"
  (declare (indent 1))
  `(when (lame//cancel-current-task-p)
     (when lame-task-buffer
       (kill-buffer lame-task-buffer))
     (lame//reset)
     (lame//prepare-task-buffer (taskname description))
     ,@body))

(defun lame//cancel-current-task-p ()
  "Check if a task is currently running.
If a task is running, ask the user to cancel the current task
then the decision of the user is returned.
If no task is running t is returned."
  (or (not lame-task) (y-or-n-p "Cancel currently running task? ")))

(defun lame//prepare-task-buffer (taskname description)
  "Create a buffer to capture the progress of task called TASKNAME.
Insert the given DESCRIPTION of the task in the buffer."
  (let* ((taskbuffername (lame//get-task-buffer-name taskname))
         (buffer (get-buffer-create taskbuffername)))
    (with-current-buffer buffer
      (setq buffer-read-only t)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (org-mode)
        (lame//initial-buffer-contents taskname description)))
    (setq lame-task-buffer buffer)))

(defun lame//reset ()
  "Clear all lame global variables.
Reset the following variables:
LAME-TASK
LAME-TASK-BUFFER
LAME_CONTINUE-CALLBACK"
  (setq lame-task nil
        lame-task-buffer nil
        lame-continue-callback nil))

(defun lame//format-task-buffer-name (taskname)
  "Return a formated buffer name for the given TASKNAME."
  (format "Task: %s" taskname))

(defun lame//initial-buffer-contents (taskname description)
  "Fill the buffer with the initial contents.
E.g. title for the given TASKNAME and DESCRIPTION."
  (org-insert-heading nil nil t)
  (insert (concat
           (lame//format-task-title taskname) "\n"
           (lame//format-task-description description))))

(defun lame//format-task-title (taskname)
  "Return a formatted task title for the given TASKNAME."
  (format "Task: %s" taskname))

(defun lame//format-task-description (description)
  "Return a formatted text for the given DESCRIPTION."
  description)

(defmacro lame/step (stepname description &rest body)
  "A step with STEPNAME of a task.
Add DESCRIPTION to the `lame-task-buffer'.
Then execute BODY."
  (declare (indent 1))
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
  (lame//reset))

(defmacro lame/defer (before after)
  "Evaluate BEFORE and return it's return value.
Sets `lame-continue-callback' to evaluate AFTER."
  (declare (indent 1))
  `(progn
     (setq lame-continue-callback
           (lambda () ,after))
     ,before))

(defmacro lame/show-buffer (msg buffer &rest body)
  "Show MSG and show BUFFER.
BODY will get executed when calling `lame/continue'."
  (declare (indent 1))
  `(lame/confirm
    ,msg
    (switch-to-buffer ,buffer)
     ,@body))

(defmacro lame/confirm (prompt before &rest after)
  "Ask the user to continue with PROMPT after executing BEFORE.
If the user agrees execute AFTER else set AFTER as `lame-continue-callback'."
  (declare (indent 1))
  `(progn
     ,before
     (if (y-or-n-p ,prompt)
         (progn ,@after)
       (setq lame-continue-callback
             (lambda () ,@after)))))

(defmacro lame/switch-branch (branch repo &rest after)
  "Checkout BRANCH in a git REPO.
Execute AFTER afterwards."
  (declare (indent 1))
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
  (declare (indent 1))
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
