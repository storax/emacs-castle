;;; init-benchmarking --- Measure startup time

;;; Commentary:

;;; Code:

(add-to-list 'load-path (concat
			 (file-name-as-directory
			  (expand-file-name "user-lisp" user-emacs-directory))
			 "benchmark-init"))
(require 'benchmark-init-loaddefs)
(benchmark-init/activate)

(provide 'init-benchmarking)
;;; init-benchmarking ends here
