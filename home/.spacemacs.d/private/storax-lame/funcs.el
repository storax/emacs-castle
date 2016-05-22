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
  "The curren task buffer.")

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
  "Create a task with TASKNAME and DESCRIPTION and execute BODY."
  `(let ((taskbuffername (format "Task: %s" ,taskname)))
     (when (or (not lame-task) (y-or-n-p "Cancel currently running task?"))
       (display-buffer taskbuffername '(display-buffer-reuse-window))
       (setq lame-task ,taskname
             lame-task-buffer (get-buffer taskbuffername)
             lame-continue-callback nil)
       (message "asdfasdf %s %s" lame-task-buffer taskbuffername)
       (with-current-buffer lame-task-buffer
         (setq buffer-read-only t)
         (let ((inhibit-read-only t))
           (org-mode)
           (goto-char (point-max))
           (org-insert-heading nil nil t)
           (insert (format "Task: %s\n%s" ,taskname ,description))))
       ,@body)))

(defmacro lame/step (stepname description &rest body)
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
  (setq lame-task nil
        lame-continue-callback nil
        lame-task-buffer nil))

(defmacro lame/defer (before after)
  "Evaluate BEFORE and return it's return value.
Sets `lame-continue-callback' to evaluate AFTER."
  `(progn
     (setq lame-continue-callback
           (lambda () ,@after))
     ,before))

(defmacro lame/show-buffer (msg buffer &rest body)
  "Show MESSAGE and show to BUFFER.
BODY will get executed when calling `lame/continue'."
  `(progn
     (switch-to-buffer ,buffer)
     (lame/defer (message ,msg) ,body)))

(defun blabla ()
  "asdf."
  (interactive)
  (lame/task
   "testrun"
   "Try all the different macros to see how they behave."
   (lame/step
    "Show Buffer"
    "Call lame/show-buffer. It should display msg and a buffer.
Calling lame/continue will execute body."
    (lame/show-buffer
     "Interesting message"
     "funcs.el"
     (lame/step
      "finished"
      "Congrats!!!!!"
      (lame/done))))))

;;; funcs.el ends here
