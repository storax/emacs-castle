;;; packages.el --- storax-org layer packages file for Spacemacs.
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

(defconst storax-org-packages
  '(org orgbox ox-rst))

(defun storax-org/init-orgbox ()
  (use-package orgbox))

(defun storax-org/init-ox-rst ()
  (use-package ox-rst))

(defun storax-org/setup-org ()
  (setq org-modules
        '(org-bibtex
          org-docview
          org-depend
          org-gnus
          org-habit
          org-id
          org-info))
  (setq org-hide-emphasis-markers t
        org-pretty-entities t)
  (setq org-startup-indented t)
  (setq org-cycle-separator-lines 0)
  (setq org-blank-before-new-entry (quote ((heading)
                                           (plain-list-item . auto))))
  (setq org-enforce-todo-dependencies t)
  (setq org-reverse-note-order nil)
  (setq org-yank-adjusted-subtrees t)
  (setq org-deadline-warning-days 30)
  (setq org-src-window-setup 'current-window)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  (setq org-log-state-notes-insert-after-drawers nil)
  (setq org-clock-sound storax-org-warning-sound)
  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
          (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "PHONE" "MEETING")))
  (setq org-todo-keyword-faces
        '(("TODO" :foreground "OrangeRed" :weight bold)
          ("NEXT" :foreground "DeepSkyBlue" :weight bold)
          ("DONE" :foreground "SpringGreen" :weight bold)
          ("WAITING" :foreground "Orange" :weight bold)
          ("HOLD" :foreground "HotPink" :weight bold)
          ("CANCELLED" :foreground "DarkGray" :weight bold)
          ("MEETING" :foreground "MediumSeaGreen" :weight bold)))
  (setq org-use-fast-todo-selection t
        org-treat-S-cursor-todo-selection-as-state-change nil)
  (setq org-todo-state-tags-triggers
        '(("CANCELLED" ("CANCELLED" . t))
          ("WAITING" ("WAITING" . t))
          ("HOLD" ("WAITING") ("HOLD" . t))
          (done ("WAITING") ("HOLD"))
          ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
          ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
          ("DONE" ("WAITING") ("CANCELLED") ("HOLD"))))
  (setq org-directory "~/Documents/org"
        org-default-notes-file "~/Documents/refile.org"
        org-agenda-files '("~/Documents/org"))
  (setq org-capture-templates
        '(("t" "todo" entry (file "~/Documents/org/refile.org")
           "* TODO %?\n" :clock-in t :clock-resume t)
          ("r" "respond" entry (file "~/Documents/org/refile.org")
           "* NEXT Respond to %:from on %:subject\nSCHEDULED: %t\n" :clock-in t :clock-resume t :immediate-finish t)
          ("n" "note" entry (file "~/Documents/org/notes.org")
           "* %? :NOTE:\n" :clock-in t :clock-resume t)
          ("j" "Journal" entry (file+datetree "~/Documents/org/diary.org")
           "* %?\n%U\n" :clock-in t :clock-resume t)
          ("w" "org-protocol" entry (file "~/Documents/org/refile.org")
           "* TODO Review %c\n%U\n" :immediate-finish t)
          ("m" "Meeting" entry (file "~/Documents/org/refile.org")
           "* MEETING with %? :MEETING:\n%U" :clock-in t :clock-resume t)
          ("h" "Habit" entry (file "~/Documents/org/refile.org")
           "* NEXT %?\nSCHEDULED: %(format-time-string \"%<<%Y-%m-%d %a .+1d/3d>>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n")))
  ;; Targets include this file and any file contributing to the agenda - up to 9 levels deep
  (setq org-refile-targets (quote ((nil :maxlevel . 9)
                                   (org-agenda-files :maxlevel . 9)))
        org-refile-use-outline-path t
        org-outline-path-complete-in-steps nil
        org-refile-allow-creating-parent-nodes 'confirm
        org-refile-target-verify-function 'storax/org-verify-refile-target
        org-agenda-dim-blocked-tasks nil
        org-agenda-compact-blocks t
        org-enforce-todo-dependencies t
        org-agenda-auto-exclude-function 'storax/org-auto-exclude-function)
  (setq org-agenda-custom-commands
        '(("N" "Notes" tags "NOTE"
           ((org-agenda-overriding-header "Notes")
            (org-tags-match-list-sublevels t)))
          ("h" "Habits" tags-todo "STYLE=\"habit\""
           ((org-agenda-overriding-header "Habits")
            (org-agenda-sorting-strategy
             '(todo-state-down effort-up category-keep))))
          (" " "Agenda"
           ((agenda "" nil)
            (tags "REFILE"
                  ((org-agenda-overriding-header "Tasks to Refile")
                   (org-tags-match-list-sublevels nil)))
            (tags-todo "-CANCELLED/!"
                       ((org-agenda-overriding-header "Stuck Projects")
                        (org-agenda-skip-function 'storax/org-skip-non-stuck-projects)
                        (org-agenda-sorting-strategy
                         '(category-keep))))
            (tags-todo "-HOLD-CANCELLED/!"
                       ((org-agenda-overriding-header "Projects")
                        (org-agenda-skip-function 'storax/org-skip-non-projects)
                        (org-tags-match-list-sublevels 'indented)
                        (org-agenda-sorting-strategy
                         '(category-keep))))
            (tags-todo "-CANCELLED/!NEXT"
                       ((org-agenda-overriding-header
                         (concat "Project Next Tasks"
                                 (if storax/org-hide-scheduled-and-waiting-next-tasks
                                     ""
                                   " (including WAITING and SCHEDULED tasks)")))
                        (org-agenda-skip-function 'storax/org-skip-projects-and-habits-and-single-tasks)
                        (org-tags-match-list-sublevels t)
                        (org-agenda-todo-ignore-scheduled storax/org-hide-scheduled-and-waiting-next-tasks)
                        (org-agenda-todo-ignore-deadlines storax/org-hide-scheduled-and-waiting-next-tasks)
                        (org-agenda-todo-ignore-with-date storax/org-hide-scheduled-and-waiting-next-tasks)
                        (org-agenda-sorting-strategy
                         '(todo-state-down effort-up category-keep))))
            (tags-todo "-REFILE-CANCELLED-WAITING-HOLD/!"
                       ((org-agenda-overriding-header
                         (concat "Project Subtasks"
                                 (if storax/org-hide-scheduled-and-waiting-next-tasks
                                     ""
                                   " (including WAITING and SCHEDULED tasks)")))
                        (org-agenda-skip-function 'storax/org-skip-non-project-tasks)
                        (org-agenda-todo-ignore-scheduled storax/org-hide-scheduled-and-waiting-next-tasks)
                        (org-agenda-todo-ignore-deadlines storax/org-hide-scheduled-and-waiting-next-tasks)
                        (org-agenda-todo-ignore-with-date storax/org-hide-scheduled-and-waiting-next-tasks)
                        (org-agenda-sorting-strategy
                         '(category-keep))))
            (tags-todo "-REFILE-CANCELLED-WAITING-HOLD/!"
                       ((org-agenda-overriding-header
                         (concat "Standalone Tasks"
                                 (if storax/org-hide-scheduled-and-waiting-next-tasks
                                     ""
                                   " (including WAITING and SCHEDULED tasks)")))
                        (org-agenda-skip-function 'storax/org-skip-project-tasks)
                        (org-agenda-todo-ignore-scheduled storax/org-hide-scheduled-and-waiting-next-tasks)
                        (org-agenda-todo-ignore-deadlines storax/org-hide-scheduled-and-waiting-next-tasks)
                        (org-agenda-todo-ignore-with-date storax/org-hide-scheduled-and-waiting-next-tasks)
                        (org-agenda-sorting-strategy
                         '(category-keep))))
            (tags-todo "-CANCELLED+WAITING|HOLD/!"
                       ((org-agenda-overriding-header
                         (concat "Waiting and Postponed Tasks"
                                 (if storax/org-hide-scheduled-and-waiting-next-tasks
                                     ""
                                   " (including WAITING and SCHEDULED tasks)")))
                        (org-agenda-skip-function 'storax/org-skip-non-tasks)
                        (org-tags-match-list-sublevels nil)
                        (org-agenda-todo-ignore-scheduled storax/org-hide-scheduled-and-waiting-next-tasks)
                        (org-agenda-todo-ignore-deadlines storax/org-hide-scheduled-and-waiting-next-tasks)))
            (tags "-REFILE/"
                  ((org-agenda-overriding-header "Tasks to Archive")
                   (org-agenda-skip-function 'storax/org-skip-non-archivable-tasks)
                   (org-tags-match-list-sublevels nil))))
           nil)))
  ;; Resume clocking task when emacs is restarted
  (org-clock-persistence-insinuate)
  ;; Show lot of clocking history so it's easy to pick items
  (setq org-clock-history-length 23)
  ;; Resume clocking task on clock-in if the clock is open
  (setq org-clock-in-resume t)
  ;; Change tasks to NEXT when clocking in
  (setq org-clock-in-switch-to-state 'storax/org-clock-in-to-next)
  ;; Separate drawers for clocking and logs
  (setq org-drawers '("PROPERTIES" "LOGBOOK"))
  ;; Save clock data and state changes and notes in the LOGBOOK drawer
  (setq org-clock-into-drawer t)
  ;; Sometimes I change tasks I'm clocking quickly - this removes clocked tasks with 0:00 duration
  (setq org-clock-out-remove-zero-time-clocks t)
  ;; Clock out when moving task to a done state
  (setq org-clock-out-when-done t)
  ;; Save the running clock and all clock history when exiting Emacs, load it on startup
  (setq org-clock-persist t)
  ;; Do not prompt to resume an active clock
  (setq org-clock-persist-query-resume nil)
  ;; Enable auto clock resolution for finding open clocks
  (setq org-clock-auto-clock-resolution 'when-no-clock-is-running)
  ;; Include current clocking task in clock reports
  (setq org-clock-report-include-clocking-task t)
  (setq storax/org-keep-clock-running nil)
  (setq org-time-stamp-rounding-minutes '(1 1))
  (setq org-agenda-clock-consistency-checks
        '(:max-duration "4:00"
                        :min-duration 0
                        :max-gap 0
                        :gap-ok-around ("4:00")))
  ;; Agenda clock report parameters
  (setq org-agenda-clockreport-parameter-plist
        '(:link t :maxlevel 5 :fileskip0 t :compact t :narrow 80))
  ;; Set default column view headings: Task Effort Clock_Summary
  (setq org-columns-default-format "%80ITEM(Task) %10Effort(Effort){:} %10CLOCKSUM")
  ;; Agenda log mode items to display (closed and state changes by default)
  (setq org-agenda-log-mode-items '(closed state))
  ;; Tags with fast selection keys
  (setq org-tag-alist
        '((:startgroup)
          ("@work" . ?e)
          ("@home" . ?H)
          (:endgroup)
          (:startgroup)
          ("TRIAGE" . ?t)
          ("IMPL" . ?i)
          ("CODEREVIEW" . ?r)
          (:endgroup)
          ("WAITING" . ?w)
          ("HOLD" . ?h)
          ("PERSONAL" . ?P)
          ("WORK" . ?W)
          ("NOTE" . ?n)
          ("CANCELLED" . ?c)))

  ;; Allow setting single tags without the menu
  (setq org-fast-tag-selection-single-key nil)
  ;; For tag searches ignore tasks with scheduled and deadline dates
  (setq org-agenda-tags-todo-honor-ignore-options t)
  ;;(setq org-agenda-span 'day)
  (setq org-stuck-projects '("" nil nil ""))
  (setq org-archive-mark-done nil)
  (setq org-archive-location "%s_archive::* Archived Tasks")
  (setq org-alphabetical-lists t)
  ;;(setq org-ditaa-jar-path "~/git/org-mode/contrib/scripts/ditaa.jar")
  ;;(setq org-plantuml-jar-path "~/java/plantuml.jar")

  ;;TODO(setq org-ditaa-jar-path "~/git/org-mode/contrib/scripts/ditaa.jar")
  ;;TODO(setq org-plantuml-jar-path "~/java/plantuml.jar")
  ;; Make babel results blocks lowercase
  (setq org-babel-results-keyword "results")
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (dot . t)
     (ditaa . t)
     (python . t)
     (ruby . t)
     (gnuplot . t)
     (clojure . t)
     (sh . t)
     (ledger . t)
     (org . t)
     (plantuml . t)
     (latex . t)))
  ;; Do not prompt to confirm evaluation
  ;; This may be dangerous - make sure you understand the consequences
  ;; of setting this -- see the docstring for details
  (setq org-confirm-babel-evaluate nil)
  (add-to-list 'org-src-lang-modes '("plantuml" . fundamental))
  ;; Don't enable this because it breaks access to emacs from Android phone
  (setq org-startup-with-inline-images nil)
  ;; Inline images in HTML instead of producting links to the image
  (setq org-html-inline-images t)
  ;; Do not use sub or superscripts - I currently don't need this functionality in my documents
  (setq org-export-with-sub-superscripts nil)
  ;; Do not generate internal css formatting for HTML exports
  (setq org-export-htmlize-output-type (quote css))
  ;; Export with LaTeX fragments
  (setq org-export-with-LaTeX-fragments t)
  ;; Increase default number of headings to export
  (setq org-export-headline-levels 6
        org-export-with-tags nil)
  (setq org-publish-project-alist
        `(;; Miscellaneous pages for other websites
          ;; org are the org-files that generate the content
          ("org-org"
           :base-directory "~/Documents/org/"
           :publishing-directory "~/Documents/org-publish"
           :recursive t
           :section-numbers nil
           :table-of-contents nil
           :base-extension "org"
           :plain-source t
           :htmlized-source t
           :html-head "<link rel=\"stylesheet\" type=\"text/css\" href=\"styles/readtheorg/css/htmlize.css\"/>
<link rel=\"stylesheet\" type=\"text/css\" href=\"styles/readtheorg/css/readtheorg.css\"/>
<script src=\"styles/lib/js/jquery/2.1.3/jquery.min.js\"></script>
<script src=\"styles/lib/js/bootstrap/3.3.4/bootstrap.min.js\"></script>
<script type=\"text/javascript\" src=\"styles/lib/js/jquery.stickytableheaders.min.js\"></script>
<script type=\"text/javascript\" src=\"styles/readtheorg/js/readtheorg.js\"></script>"
           :publishing-function org-html-publish-to-html
           :style-include-default nil
           :author-info t
           :creator-info t)
          ("org-extra"
           :base-directory ,storax-org-rtd-theme-path
           :publishing-directory "~/Documents/org-publish"
           :base-extension "css\\|pdf\\|png\\|jpg\\|gif\\|org\\|js"
           :publishing-function org-publish-attachment
           :recursive t
           :author nil)
          ("org"
           :components ("org-org" "org-extra"))))
  (setq org-latex-listings t)
  (setq org-html-xml-declaration
        '(("html" . "")
          ("was-html" . "<?xml version=\"1.0\" encoding=\"%s\"?>")
          ("php" . "<?php echo \"<?xml version=\\\"1.0\\\" encoding=\\\"%s\\\" ?>\"; ?>")))
  (setq org-export-allow-BIND t)
  ;; Keep tasks with dates on the global todo lists
  (setq org-agenda-todo-ignore-with-date nil)
  ;; Keep tasks with deadlines on the global todo lists
  (setq org-agenda-todo-ignore-deadlines nil)
  ;; Keep tasks with scheduled dates on the global todo lists
  (setq org-agenda-todo-ignore-scheduled nil)
  ;; Keep tasks with timestamps on the global todo lists
  (setq org-agenda-todo-ignore-timestamp nil)
  ;; Remove completed deadline tasks from the agenda view
  (setq org-agenda-skip-deadline-if-done t)
  ;; Remove completed scheduled tasks from the agenda view
  (setq org-agenda-skip-scheduled-if-done t)
  ;; Remove completed items from search results
  (setq org-agenda-skip-timestamp-if-done t)
  (setq org-agenda-include-diary nil)
  (setq org-agenda-diary-file "~/Documents/org/diary.org")
  (setq org-agenda-insert-diary-extract-time t)
  ;; Include agenda archive files when searching for things
  (setq org-agenda-text-search-extra-files '(agenda-archives))
  ;; Show all future entries for repeating tasks
  (setq org-agenda-repeating-timestamp-show-all t)
  ;; Show all agenda dates - even if they are empty
  (setq org-agenda-show-all-dates t)
  ;; Sorting order for tasks on the agenda
  (setq org-agenda-sorting-strategy
        '((agenda habit-down time-up user-defined-up effort-up category-keep)
          (todo category-up effort-up)
          (tags category-up effort-up)
          (search category-up)))
  ;; Start the weekly agenda on Monday
  (setq org-agenda-start-on-weekday 1)
  ;; Enable display of the time grid so we can see the marker for the current time
  (setq org-agenda-time-grid '((daily today remove-match)
                               #("----------------" 0 16 (org-heading t))
                               (0900 1100 1300 1500 1700)))
  ;; Display tags farther right
  (setq org-agenda-tags-column -102)
  (setq org-agenda-cmp-user-defined 'storax/org-agenda-sort)
  ;; position the habit graph on the agenda to the right of the default
  (setq org-habit-graph-column 50)
  (setq org-return-follows-link t)
  (setq org-read-date-prefer-future 'time)
  (setq org-cycle-include-plain-lists t)
  (setq org-time-clocksum-format
        '(:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t)))

(defun storax-org/init-org ()
  (use-package org
    :defer t
    :init
    (spacemacs/declare-prefix "o" "org" "org-mode")
    (spacemacs/declare-prefix "oa" "agenda" "org agenda")
    (spacemacs/declare-prefix "or" "report" "org report")
    (spacemacs/declare-prefix "op" "publish" "org publish")
    (spacemacs/declare-prefix "ov" "visual" "org toggle visual")
    (progn
      (spacemacs/set-leader-keys "oaa" 'org-agenda)
      (spacemacs/set-leader-keys "oac" 'org-cycle-agenda-files)
      (spacemacs/set-leader-keys "ob" 'org-switchb)
      (spacemacs/set-leader-keys "oc" 'org-capture)
      (spacemacs/set-leader-keys "oC" 'calendar)
      (spacemacs/set-leader-keys "ol" 'org-store-link)
      (spacemacs/set-leader-keys "on" 'storax/org-toggle-next-task-display)
      (spacemacs/set-leader-keys "op SPC" 'org-publish)
      (spacemacs/set-leader-keys "opa" 'org-publish-all)
      (spacemacs/set-leader-keys "opf" 'org-publish-current-file)
      (spacemacs/set-leader-keys "opp" 'storax/org-save-then-publish)
      (spacemacs/set-leader-keys "opP" 'org-publish-project)
      (spacemacs/set-leader-keys "org" 'org-clock-goto)
      (spacemacs/set-leader-keys "ori" 'org-clock-in)
      (spacemacs/set-leader-keys "ork" 'storax/org-punch-out)
      (spacemacs/set-leader-keys "oro" 'org-clock-out)
      (spacemacs/set-leader-keys "orp" 'storax/org-punch-in)
      (spacemacs/set-leader-keys "orr" 'org-resolve-clocks)
      (spacemacs/set-leader-keys "ot" 'org-toc-show)
      (spacemacs/set-leader-keys "ot" 'org-toc-show)
      (spacemacs/set-leader-keys "ova" 'show-all)
      (spacemacs/set-leader-keys "ovh" 'storax/org-hide-other))
      (spacemacs/set-leader-keys "ovi" 'org-toggle-inline-images)
      (spacemacs/set-leader-keys "ovl" 'org-toggle-link-display)
      (spacemacs/set-leader-keys "ovt" 'storax/set-truncate-lines)
    :config
    (progn
      (storax-org/setup-org)
      (add-hook 'org-clock-out-hook 'org-clock-remove-empty-clock-drawer 'append)
      (add-hook 'org-clock-out-hook 'storax/org-clock-out-maybe 'append)
      (add-hook 'org-babel-after-execute-hook 'storax/org-display-inline-images 'append)
      ;; Rebuild the reminders everytime the agenda is displayed
      (add-hook 'org-finalize-agenda-hook 'storax/org-agenda-to-appt 'append)
      (add-hook 'after-init-hook 'storax/org-agenda-to-appt)
      (add-hook 'after-init-hook (lambda () (load-file (concat (file-name-directory load-file-name) "appt.el"))))
      (appt-activate t)
      (run-at-time "24:01" nil 'storax/org-agenda-to-appt)
      (run-at-time "06:00" 86400 '(lambda () (setq org-habit-show-habits t)))
      (run-at-time "00:59" 3600 'org-save-all-org-buffers)
      (org-babel-lob-ingest storax-org-lob-file))))

;;; packages.el ends here
