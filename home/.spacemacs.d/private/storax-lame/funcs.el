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
(require 'cl-lib)
(require 'async)

(defgroup lame nil
  "Do lame things more efficiently."
  :group 'applications)

(defcustom lame-task-prefix "Task: "
  "Prefix for the task name."
  :type 'string
  :group 'lame)

(defcustom lame-step-prefix "Step: "
  "Prefix for the step name."
  :type 'string
  :group 'lame)

(defcustom lame-running-task-keyword "TODO"
  "Keyword to use for current running tasks."
  :type 'string
  :group 'lame)

(defcustom lame-current-step-keyword "NEXT"
  "Keyword to use for the step that is currently running."
  :type 'string
  :group 'lame)

(defcustom lame-done-keyword "DONE"
  "Keyword for done steps or tasks."
  :type 'string
  :group 'lame)

(cl-defstruct lame-task
  name description)

(cl-defstruct lame-step
  name description)

(defvar lame-continue-callback nil
  "Callback function continue current lame session.")

(defvar lame-current-task nil
  "The current task.")

(defvar lame-current-step nil
  "The current task.")

(defvar lame-task-buffer nil
  "The current task buffer.")

(defvar lame-edit-buffer nil
  "The current edit buffer.")

(defvar lame-process nil
  "The current running lame process.")
(defun lame/interactive-args (func)
  "Get the interactive args of FUNC."
  (advice-eval-interactive-spec (cadr (interactive-form func))))

(defmacro lame/let-interactive (spec &rest body)
  "Get the interactive args of function in SPEC and let-bind them to SYMBOLS.
Then evaluate BODY.
SYMBOLS should be a list the length of the interactive args of FUNC.

\(fn (FUNC SYMBOLS) BODY...)"
  (declare (indent 0))
  (let* ((func (car spec))
         (symbols (cadr spec))
         (args (lame/interactive-args func))
         (letlist (mapcar* #'list (setcdr (last symbols) symbols) args))) ;zipd
    `(let ,letlist
       ,@body)))

(cl-defmacro lame/task (&key (name "Unnamed Task")
                             description
                             task)
  "Create a task called NAME.

Create a task buffer to track the progress.
The initial DESCRIPTION is inserted.
Execute PREPARATION then ask the user to execute the task.
Once the user continues, execute TASK.
The task is marked as finished after calling `lame/done'"
  (declare (indent 0))
  `(when (lame//cancel-current-task-p)
     (when lame-task-buffer
       (kill-buffer lame-task-buffer))
     (lame//reset)
     (setq lame-current-task (make-lame-task :name ,name :description ,description))
     (lame//prepare-task-buffer lame-current-task)
     ,task))

(defun lame//cancel-current-task-p ()
  "Check if a task is currently running.
If a task is running, ask the user to cancel the current task
then the decision of the user is returned.
If no task is running t is returned."
  (or (not lame-current-task) (y-or-n-p (format "Cancel currently running task \"%s\"? "
                                                (lame-task-name lame-current-task)))))

(defun lame//prepare-task-buffer (task)
  "Create a buffer to capture the progress of the given TASK."
  (let* ((name (lame-task-name task))
         (taskbuffername (lame//format-task-buffer-name name))
         (buffer (get-buffer-create taskbuffername)))
    (with-current-buffer buffer
      (setq buffer-read-only t)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (org-mode)
        (lame//initial-buffer-contents task)))
    (setq lame-task-buffer buffer)))

(defun lame//reset ()
  "Clear all lame global variables.
Reset the following variables:
- `lame-current-task'
- `lame-current-step'
- `lame-task-buffer'
- `lame-continue-callback'"
  (setq lame-current-task nil
        lame-current-step nil
        lame-task-buffer nil
        lame-continue-callback nil))

(defun lame//format-task-buffer-name (taskname)
  "Return a formated buffer name for the given TASKNAME."
  (concat lame-task-prefix taskname))

(defun lame//initial-buffer-contents (task)
  "Fill the buffer with the initial contents for the given TASK."
  (org-insert-heading nil nil t)
  (let ((description (lame-task-description task)))
    (insert (concat
             (lame//format-task-title task lame-running-task-keyword) (when description "\n")
             (lame//format-task-description description)))))

(defun lame//add-field (text value)
  "Add field to TEXT with the given VALUE."
  (put-text-property 0 (length text) 'field value text))

(defun lame//format-task-title (task status)
  "Return a formatted TASK title with STATUS."
  (let* ((name (lame-task-name task))
         (text (concat status " " lame-task-prefix name)))
    (lame//add-field text task)
    text))

(defun lame//format-step-title (step status)
  "Return a formatted STEP title with STATUS."
  (let* ((name (lame-step-name step))
         (text (concat status " " lame-step-prefix name)))
    (lame//add-field text step)
    text))

(defun lame//format-task-description (description)
  "Return a formatted text for the given DESCRIPTION."
  description)

(defun lame//set-status (object status textfunc)
  "Replace the field of OBJECT with the new text.
STATUS is used for TEXTFUNC.
It's called with OBJECT and STATUS."
  (with-current-buffer lame-task-buffer
    (let* ((start (text-property-any (point-min) (point-max) 'field object))
           (end (field-end start t))
           (newtext (funcall textfunc object status))
           (inhibit-read-only t))
      (delete-region start end)
      (goto-char start)
      (insert newtext))))

(defun lame//set-task-status (task status)
  "For the given TASK set the STATUS."
  (lame//set-status task status 'lame//format-task-title))

(defun lame//set-step-status (step status)
  "For the given STEP set the STATUS."
  (lame//set-status step status 'lame//format-step-title))

(cl-defmacro lame/step (&key (name "Unnamed Step")
                             description
                             preparation
                             step)
  "A step with NAME of a task.
Add DESCRIPTION to the `lame-task-buffer'.
Then execute preparation.
When the user continues execute step."
  (declare (indent 0))
  `(progn
     (when lame-current-step
       (lame//set-step-status lame-current-step lame-done-keyword))
     (display-buffer lame-task-buffer '(display-buffer-reuse-window))
     (with-current-buffer lame-task-buffer
       (setq lame-current-step (make-lame-step :name ,name :description ,description))
       (let ((inhibit-read-only t)
             (desc ,description)
             (title (lame//format-step-title lame-current-step lame-current-step-keyword)))
         (goto-char (point-max))
         (org-insert-heading nil nil t)
         (org-demote)
         (insert (concat title (when desc "\n") desc))))
     (lame/defer
       :before ,preparation
       :after ,step)))

(defun lame/continue ()
  "Continue the current lame session."
  (interactive)
  (when lame-continue-callback
    (funcall lame-continue-callback)))

(defun lame/done ()
  "Call when you are done with the current task."
  (message "Task %s completed!" (lame-task-name lame-current-task))
  (when lame-current-step
    (lame//set-step-status lame-current-step lame-done-keyword))
  (lame//set-task-status lame-current-task lame-done-keyword)
  (lame//reset))

(cl-defmacro lame/defer (&key before after)
  "Evaluate BEFORE and return it's return value.
Sets `lame-continue-callback' to evaluate AFTER."
  (declare (indent 0))
  `(progn
     (setq lame-continue-callback (lambda () ,after))
     ,before))

(cl-defmacro lame/confirm (prompt &key before after)
  "Ask the user to continue with PROMPT after executing BEFORE.
If the user agrees execute AFTER else set AFTER as `lame-continue-callback'."
  (declare (indent 0))
  `(lame/defer
     :before (progn ,before
                    (when (y-or-n-p ,prompt)
                      (funcall lame-continue-callback)))
     :after ,after))

(put 'lame/confirm 'lisp-indent-function 'defun)

(cl-defmacro lame/show-buffer (buffer &key (msg "Continue now?") next)
  "Show MSG and show BUFFER.
BODY will get executed when calling `lame/continue'."
  (declare (indent 0))
  `(lame/confirm (or ,msg "Continue now?")
     :before (switch-to-buffer ,buffer)
     :after ,next))

(put 'lame/show-buffer 'lisp-indent-function 'defun)

(cl-defmacro lame/switch-to-branch (branch &key repo next)
  "Checkout BRANCH in a git REPO.
Execute NEXT afterwards."
  (declare (indent 0))
  `(lame/confirm (format "Checkout branch %s now?" ,branch)
     :before (progn (magit-status ,repo))
     :after (progn
              (magit-checkout ,branch)
              ,next)))

(put 'lame/switch-to-branch 'lisp-indent-function 'defun)

(cl-defmacro lame/edit-text (&key (msg "Please edit the text.")
                                  (var 'lame--edit-text-var)
                                  text mode next)
  "Show MSG and an edit buffer.
Once the user calls `lame-continue' VAR is set with the current text of the buffer.
TEXT is the initial text in the buffer.
MODE is an optional mode to activate.
Execute NEXT afterwards."
  (declare (indent 0))
  `(lame/defer
     :before (progn
               (setq lame-edit-buffer (switch-to-buffer-other-window (format "*lame: edit %s" ',var)))
               (erase-buffer)
               (insert ,text)
               (when ',mode
                 (funcall ',mode))
               (message ,msg))
     :after (progn
              (with-current-buffer lame-edit-buffer
                (setq ,var (buffer-string))
                (when (get-buffer-window)
                  (delete-window (get-buffer-window)))
                (kill-buffer lame-edit-buffer)
                (setq lame-edit-buffer nil))
              ,next)))

(cl-defun lame/add-text-to-description (text &key wrap lang)
  "Add TEXT to the description of the current task/step.
If WRAP is non-nil wrap the text in a #+BEGIN_<wrap> #+END_<wrap> block.
If LANG is non-nil add it like this:  #+BEGIN_<wrap> <lang>."
  (with-current-buffer lame-task-buffer
    (let* ((inhibit-read-only t)
           (p (point-max)))
      (goto-char p)
      (if wrap
          (progn
            (insert
             (format
              "\n#+BEGIN_%s%s\n%s\n#+END_%s"
              wrap (if lang (concat " " lang) "") text wrap))
            (goto-char p)
            (while (re-search-forward "^\*+ " nil t)
              (replace-match ",\\&")))
        (insert (concat "\n" text))))))

(put 'lame/add-text-to-description 'lisp-indent-function 'defun)

(cl-defmacro lame/execute-shell (command &key name (shell "sh") dir show-output stay on-error next)
  "Execute COMMAND in a shell.
See `lame/execute'."
  (macroexpand
   `(lame/execute ,shell :args ("-c" ,command)
      :name ,name :dir ,dir
      :show-output ,show-output :stay ,stay
      :on-error ,on-error
      :next ,next)))

(put 'lame/execute-shell 'lisp-indent-function 'defun)

(cl-defmacro lame/execute (command &key name args dir show-output stay on-error next)
  "Execute the given COMMAND.
NAME for the process buffer.
ARGS is a list of arguments for the command.
DIR is the working directory for the command.
If non-nil SHOW-OUTPUT will display the process buffer right away.
ON-ERROR is the code to execute when the command fails.
The process buffer will be displayed anyway.
NEXT will be executed after the command sucessfully finishes.
NEXT will also be the `lame-continue-callback' if something goes wrong."
  (let ((on-error (or on-error '(message "Something went wrong.")))
        (name (or name (concat command " " (mapconcat 'identity args " ")))))
    `(progn
       (setq lame-continue-callback
             (lambda (&optional proc)
               (let ((nextfn (lambda () ,next))
                     (proc (or proc lame-process))
                     (buf (process-buffer proc)))
                 (if (process-live-p proc)
                     (message "Process hasn't finished.")
                   (progn
                     (setq lame-continue-callback nextfn)
                     (if (> (process-exit-status proc) 0)
                         (progn
                           (if (eq buf (current-buffer))
                               (switch-to-buffer buf)
                             (switch-to-buffer-other-window buf))
                           (setq buffer-read-only t)
                           ,on-error)
                       (prog1
                           (funcall lame-continue-callback)
                         (unless ,stay (kill-buffer buf)))))))))
       (let ((default-directory (or ,dir default-directory)))
         (setq lame-process
               (lame//async-start-process
                ,name
                ,command
                lame-continue-callback ,@args))
         (when ,show-output
           (switch-to-buffer-other-window (process-buffer lame-process)))))))

(put 'lame/execute'lisp-indent-function 'defun)

(defun lame//async-start-process (name program finish-func &rest program-args)
  "Start the executable PROGRAM asynchronously.  See `async-start'.
PROGRAM is passed PROGRAM-ARGS, calling FINISH-FUNC with the
process object when done.  If FINISH-FUNC is nil, the future
object will return the process object when the program is
finished.  Set DEFAULT-DIRECTORY to change PROGRAM's current
working directory."
  (let* ((buf (generate-new-buffer (concat "*" name "*")))
         (proc (let ((process-connection-type nil))
                 (apply #'start-process name buf program program-args))))
    (with-current-buffer buf
      (set (make-local-variable 'async-callback) finish-func)
      (set-process-sentinel proc #'lame//async-when-done)
      (unless (string= name "emacs")
        (set (make-local-variable 'async-callback-for-process) t))
      proc)))

(defun lame//async-when-done (proc &optional _change)
  "Process sentinel used to retrieve the value from the child process."
  (when (eq 'exit (process-status proc))
    (with-current-buffer (process-buffer proc)
      (let ((async-current-process proc))
        (if async-callback-for-process
            (if async-callback
                (funcall async-callback proc)
              (set (make-local-variable 'async-callback-value) proc)
              (set (make-local-variable 'async-callback-value-set) t))
          (goto-char (point-max))
          (backward-sexp)
          (async-handle-result async-callback (read (current-buffer))
                               (current-buffer)))))))

(cl-defun lame/add-process-output (&key (wrap "EXAMPLE") lang ansi)
  "Add process output to task buffer.
WRAP by default in EXAMPLE block.
Use LANG if non-nil.
If ansi is non-nil, apply ansi color codes first."
  (lame/add-text-to-description
    (with-current-buffer (process-buffer lame-process)
      (if ansi
          (ansi-color-apply (buffer-string))
        (buffer-string)))
    :wrap wrap :lang lang))

(defun lame-test ()
  "Test lame functions."
  (interactive)
  (eval
   '(lame/task
      :name "Lame Test"
      :description "Test lame functionality."
      :task
      (progn
        (message "Some message")
        (lame/add-text-to-description "Execute steps")
        (lame/step
          :name "Edit text"
          :description "Edit some example test."
          :step
          (lame/edit-text
            :msg "For the fun" :text "Edit me!" :var lame--test-var
            :next
            (progn
              (lame/add-text-to-description lame--test-var :wrap "EXAMPLE")
              (lame/step
                :name "Switch Branch"
                :description "Switch to branch Master."
                :step
                (lame/switch-to-branch "master"
                  :next
                  (lame/step
                    :name "Execute shell"
                    :description "Execute awesome shell command"
                    :preparation (lame/add-text-to-description "curl wttr.in/london" :wrap "SRC" :lang "sh")
                    :step
                    (lame/execute-shell "curl -s wttr.in/london"
                      :show-output t
                      :next
                      (progn
                        (lame/add-process-output :ansi t)
                        (lame/step
                          :name "Finish task"
                          :description "We end this task."
                          :step (lame/done))))))))))))))

;;; funcs.el ends here
