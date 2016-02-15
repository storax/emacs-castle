;;; benchmark-init-loaddefs.el
;;
;;; Code:

(autoload 'benchmark-init/activate (expand-file-name "user-lisp/benchmark-init/benchmark-init" user-emacs-directory) "\
Activate benchmark-init and start collecting data.

\(fn)" t nil)

(autoload 'benchmark-init/show-durations-tabulated (expand-file-name "user-lisp/benchmark-init/benchmark-init-modes" user-emacs-directory) "\
Show the benchmark results in a sorted table.

\(fn)" t nil)

(autoload 'benchmark-init/show-durations-tree (expand-file-name "user-lisp/benchmark-init/benchmark-init-modes" user-emacs-directory) "\
Show durations in call-tree.

\(fn)" t nil)

(provide 'benchmark-init-loaddefs)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; benchmark-init-loaddefs.el ends here
