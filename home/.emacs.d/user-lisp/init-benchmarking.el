;;; init-benchmarking --- Measure startup time

;;; Commentary:

;;; Code:

(add-to-list 'load-path "~/.emacs.d/lisp/benchmark-init")
(require 'benchmark-init-loaddefs)
(benchmark-init/activate)

(provide 'init-benchmarking)
;;; init-benchmarking ends here
