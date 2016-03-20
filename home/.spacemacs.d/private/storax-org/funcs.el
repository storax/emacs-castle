;;; funcs.el --- storax-org layer functions file for Spacemacs.
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

(defun storax/org-hide-other ()
  (interactive)
  (save-excursion
    (org-back-to-heading 'invisible-ok)
    (hide-other)))

(defun storax/set-truncate-lines ()
  "Toggle value of `truncate-lines' and refresh window display."
  (interactive)
  (setq truncate-lines (not truncate-lines)))

(defun storax/org-verify-refile-target ()
  "Exclude todo keywords with a done state from refile targets."
  (not (member (nth 2 (org-heading-components)) org-done-keywords)))

(defun storax/org-auto-exclude-function (tag)
  "Evaluate if TAG should be excluded."
  (and (cond
        ((string= tag "hold")
         t))
       (concat "-" tag)))

(defun storax/org-clock-in-to-next (kw)
  "Switch a task from KW TODO to NEXT when clocking in.
Skips capture tasks, projects, and subprojects.
Switch projects and subprojects from NEXT back to TODO"
  (when (not (and (boundp 'org-capture-mode) org-capture-mode))
    (cond
     ((and (member (org-get-todo-state) (list "TODO"))
           (storax/org-is-task-p))
      "NEXT")
     ((and (member (org-get-todo-state) (list "NEXT"))
           (storax/org-is-project-p))
      "TODO"))))

(defun storax/org-find-project-task ()
  "Move point to the parent (project) task if any."
  (save-restriction
    (widen)
    (let ((parent-task (save-excursion (org-back-to-heading 'invisible-ok) (point))))
      (while (org-up-heading-safe)
        (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
          (setq parent-task (point))))
      (goto-char parent-task)
      parent-task)))

(defun storax/org-punch-in (arg)
  "Start continuous clocking and set the default task to the selected task.
If no task is selected set the Organization task as the default task."
  (interactive "p")
  (setq storax/org-keep-clock-running t)
  (if (equal major-mode 'org-agenda-mode)
      ;;
      ;; We're in the agenda
      ;;
      (let* ((marker (org-get-at-bol 'org-hd-marker))
             (tags (org-with-point-at marker (org-get-tags-at))))
        (if (and (eq arg 4) tags)
            (org-agenda-clock-in '(16))
          (storax/org-clock-in-organization-task-as-default)))
    ;;
    ;; We are not in the agenda
    ;;
    (save-restriction
      (widen)
                                        ; Find the tags on the current task
      (if (and (equal major-mode 'org-mode) (not (org-before-first-heading-p)) (eq arg 4))
          (org-clock-in '(16))
        (storax/org-clock-in-organization-task-as-default)))))

(defun storax/org-punch-out ()
  (interactive)
  (setq storax/org-keep-clock-running nil)
  (when (org-clock-is-active)
    (org-clock-out))
  (org-agenda-remove-restriction-lock))

(defun storax/org-clock-in-default-task ()
  (save-excursion
    (org-with-point-at org-clock-default-task
      (org-clock-in))))

(defun storax/org-clock-in-parent-task ()
  "Move point to the parent (project) task if any and clock in."
  (let ((parent-task))
    (save-excursion
      (save-restriction
        (widen)
        (while (and (not parent-task) (org-up-heading-safe))
          (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
            (setq parent-task (point))))
        (if parent-task
            (org-with-point-at parent-task
              (org-clock-in))
          (when storax/org-keep-clock-running
            (storax/org-clock-in-default-task)))))))

(defun storax/org-clock-in-organization-task-as-default ()
  (interactive)
  (org-with-point-at (org-id-find storax/org-organization-task-id 'marker)
    (org-clock-in '(16))))

(defun storax/org-clock-out-maybe ()
  (when (and storax/org-keep-clock-running
             (not org-clock-clocking-in)
             (marker-buffer org-clock-default-task)
             (not org-clock-resolving-clocks-due-to-idleness))
    (storax/org-clock-in-parent-task)))

(defun storax/org-clock-in-task-by-id (id)
  "Clock in a task by ID."
  (org-with-point-at (org-id-find id 'marker)
    (org-clock-in nil)))

(defun storax/org-clock-in-last-task (arg)
  "Clock in the interrupted task if there is one.
Skip the default task and get the next one.
A prefix ARG forces clock in of the default task."
  (interactive "p")
  (let ((clock-in-to-task
         (cond
          ((eq arg 4) org-clock-default-task)
          ((and (org-clock-is-active)
                (equal org-clock-default-task (cadr org-clock-history)))
           (caddr org-clock-history))
          ((org-clock-is-active) (cadr org-clock-history))
          ((equal org-clock-default-task (car org-clock-history)) (cadr org-clock-history))
          (t (car org-clock-history)))))
    (widen)
    (org-with-point-at clock-in-to-task
      (org-clock-in nil))))

(defun storax/org-is-project-p ()
  "Any task with a todo keyword subtask."
  (save-restriction
    (widen)
    (let ((has-subtask)
          (subtree-end (save-excursion (org-end-of-subtree t)))
          (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
      (save-excursion
        (forward-line 1)
        (while (and (not has-subtask)
                    (< (point) subtree-end)
                    (re-search-forward "^\*+ " subtree-end t))
          (when (member (org-get-todo-state) org-todo-keywords-1)
            (setq has-subtask t))))
      (and is-a-task has-subtask))))

(defun storax/org-is-project-subtree-p ()
  "Any task with a todo keyword that is in a project subtree.
Callers of this function already widen the buffer view."
  (let ((task (save-excursion (org-back-to-heading 'invisible-ok)
                              (point))))
    (save-excursion
      (storax/org-find-project-task)
      (if (equal (point) task)
          nil
        t))))

(defun storax/org-is-task-p ()
  "Any task with a todo keyword and no subtask."
  (save-restriction
    (widen)
    (let ((has-subtask)
          (subtree-end (save-excursion (org-end-of-subtree t)))
          (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
      (save-excursion
        (forward-line 1)
        (while (and (not has-subtask)
                    (< (point) subtree-end)
                    (re-search-forward "^\*+ " subtree-end t))
          (when (member (org-get-todo-state) org-todo-keywords-1)
            (setq has-subtask t))))
      (and is-a-task (not has-subtask)))))

(defun storax/org-is-subproject-p ()
  "Any task which is a subtask of another project."
  (let ((is-subproject)
        (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
    (save-excursion
      (while (and (not is-subproject) (org-up-heading-safe))
        (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
          (setq is-subproject t))))
    (and is-a-task is-subproject)))

(defun storax/org-list-sublevels-for-projects-indented ()
  "Set `org-tags-match-list-sublevels' so when restricted to a subtree we list all subtasks.
This is normally used by skipping functions where this variable is already local to the agenda."
  (if (marker-buffer org-agenda-restrict-begin)
      (setq org-tags-match-list-sublevels 'indented)
    (setq org-tags-match-list-sublevels nil))
  nil)

(defun storax/org-list-sublevels-for-projects ()
  "Set org-tags-match-list-sublevels so when restricted to a subtree we list all subtasks.
  This is normally used by skipping functions where this variable is already local to the agenda."
  (if (marker-buffer org-agenda-restrict-begin)
      (setq org-tags-match-list-sublevels t)
    (setq org-tags-match-list-sublevels nil))
  nil)

(defun storax/org-toggle-next-task-display ()
  (interactive)
  (setq storax/org-hide-scheduled-and-waiting-next-tasks (not storax/org-hide-scheduled-and-waiting-next-tasks))
  (when  (equal major-mode 'org-agenda-mode)
    (org-agenda-redo))
  (message "%s WAITING and SCHEDULED NEXT Tasks" (if storax/org-hide-scheduled-and-waiting-next-tasks "Hide" "Show")))

(defun storax/org-skip-stuck-projects ()
  "Skip trees that are not stuck projects."
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (if (storax/org-is-project-p)
          (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
                 (has-next ))
            (save-excursion
              (forward-line 1)
              (while (and (not has-next) (< (point) subtree-end) (re-search-forward "^\\*+ NEXT " subtree-end t))
                (unless (member "WAITING" (org-get-tags-at))
                  (setq has-next t))))
            (if has-next
                nil
              next-headline)) ; a stuck project, has subtasks but no next task
        nil))))

(defun storax/org-skip-non-stuck-projects ()
  "Skip trees that are not stuck projects."
  ;; (storax/org-list-sublevels-for-projects-indented)
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (if (storax/org-is-project-p)
          (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
                 (has-next ))
            (save-excursion
              (forward-line 1)
              (while (and (not has-next) (< (point) subtree-end) (re-search-forward "^\\*+ NEXT " subtree-end t))
                (unless (member "WAITING" (org-get-tags-at))
                  (setq has-next t))))
            (if has-next
                next-headline
              nil)) ; a stuck project, has subtasks but no next task
        next-headline))))

(defun storax/org-skip-non-projects ()
  "Skip trees that are not projects."
  ;; (storax/org-list-sublevels-for-projects-indented)
  (if (save-excursion (storax/org-skip-non-stuck-projects))
      (save-restriction
        (widen)
        (let ((subtree-end (save-excursion (org-end-of-subtree t))))
          (cond
           ((storax/org-is-project-p)
            nil)
           ((and (storax/org-is-project-subtree-p) (not (storax/org-is-task-p)))
            nil)
           (t
            subtree-end))))
    (save-excursion (org-end-of-subtree t))))

(defun storax/org-skip-non-tasks ()
  "Show non-project tasks.
Skip project and sub-project tasks, habits, and project related tasks."
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (cond
       ((storax/org-is-task-p)
        nil)
       (t
        next-headline)))))

(defun storax/org-skip-project-trees-and-habits ()
  "Skip trees that are projects."
  (save-restriction
    (widen)
    (let ((subtree-end (save-excursion (org-end-of-subtree t))))
      (cond
       ((storax/org-is-project-p)
        subtree-end)
       ((org-is-habit-p)
        subtree-end)
       (t
        nil)))))

(defun storax/org-skip-projects-and-habits-and-single-tasks ()
  "Skip trees that are projects, tasks that are habits, single non-project tasks."
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (cond
       ((org-is-habit-p)
        next-headline)
       ((and storax/org-hide-scheduled-and-waiting-next-tasks
             (member "WAITING" (org-get-tags-at)))
        next-headline)
       ((storax/org-is-project-p)
        next-headline)
       ((and (storax/org-is-task-p) (not (storax/org-is-project-subtree-p)))
        next-headline)
       (t
        nil)))))

(defun storax/org-skip-project-tasks-maybe ()
  "Show tasks related to the current restriction.
When restricted to a project, skip project and sub project tasks,
habits, NEXT tasks, and loose tasks.
When not restricted, skip project and sub-project tasks,
habits, and project related tasks."
  (save-restriction
    (widen)
    (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
           (next-headline (save-excursion (or (outline-next-heading) (point-max))))
           (limit-to-project (marker-buffer org-agenda-restrict-begin)))
      (cond
       ((storax/org-is-project-p)
        next-headline)
       ((org-is-habit-p)
        subtree-end)
       ((and (not limit-to-project)
             (storax/org-is-project-subtree-p))
        subtree-end)
       ((and limit-to-project
             (storax/org-is-project-subtree-p)
             (member (org-get-todo-state) (list "NEXT")))
        subtree-end)
       (t
        nil)))))

(defun storax/org-skip-project-tasks ()
  "Show non-project tasks.
Skip project and sub-project tasks, habits, and project related tasks."
  (save-restriction
    (widen)
    (let* ((subtree-end (save-excursion (org-end-of-subtree t))))
      (cond
       ((storax/org-is-project-p)
        subtree-end)
       ((org-is-habit-p)
        subtree-end)
       ((storax/org-is-project-subtree-p)
        subtree-end)
       (t
        nil)))))

(defun storax/org-skip-non-project-tasks ()
  "Show project tasks.
Skip project and sub-project tasks, habits, and loose non-project tasks."
  (save-restriction
    (widen)
    (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
           (next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (cond
       ((storax/org-is-project-p)
        next-headline)
       ((org-is-habit-p)
        subtree-end)
       ((and (storax/org-is-project-subtree-p)
             (member (org-get-todo-state) (list "NEXT")))
        subtree-end)
       ((not (storax/org-is-project-subtree-p))
        subtree-end)
       (t
        nil)))))

(defun storax/org-skip-projects-and-habits ()
  "Skip trees that are projects and tasks that are habits."
  (save-restriction
    (widen)
    (let ((subtree-end (save-excursion (org-end-of-subtree t))))
      (cond
       ((storax/org-is-project-p)
        subtree-end)
       ((org-is-habit-p)
        subtree-end)
       (t
        nil)))))

(defun storax/org-skip-non-subprojects ()
  "Skip trees that are not projects."
  (let ((next-headline (save-excursion (outline-next-heading))))
    (if (storax/org-is-subproject-p)
        nil
      next-headline)))

(defun storax/org-skip-non-archivable-tasks ()
  "Skip trees that are not available for archiving."
  (save-restriction
    (widen)
    ;; Consider only tasks with done todo headings as archivable candidates
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max))))
          (subtree-end (save-excursion (org-end-of-subtree t))))
      (if (member (org-get-todo-state) org-todo-keywords-1)
          (if (member (org-get-todo-state) org-done-keywords)
              (let* ((daynr (string-to-int (format-time-string "%d" (current-time))))
                     (a-month-ago (* 60 60 24 (+ daynr 1)))
                     (last-month (format-time-string "%Y-%m-" (time-subtract (current-time) (seconds-to-time a-month-ago))))
                     (this-month (format-time-string "%Y-%m-" (current-time)))
                     (subtree-is-current (save-excursion
                                           (forward-line 1)
                                           (and (< (point) subtree-end)
                                                (re-search-forward (concat last-month "\\|" this-month) subtree-end t)))))
                (if subtree-is-current
                    subtree-end ; Has a date in this month or last month, skip it
                  nil))  ; available to archive
            (or subtree-end (point-max)))
        next-headline))))

(defun storax/org-display-inline-images ()
  (condition-case nil
      (org-display-inline-images)
    (error nil)))

(defun storax/org-save-then-publish (&optional force)
  (interactive "P")
  (save-buffer)
  (org-save-all-org-buffers)
  (let ((org-html-head-extra))
    (org-publish-current-project force)))

;;; funcs.el ends here
