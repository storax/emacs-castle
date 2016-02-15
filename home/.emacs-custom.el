;;; .emacs-custom --- Stuff set by customize

;;; Commentary:

;;; Code:
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-command-list
   (quote
    (("TeX" "%(PDF)%(tex) %(file-line-error) %(extraopts) %`%S%(PDFout)%(mode)%' %t" TeX-run-TeX nil
      (plain-tex-mode texinfo-mode ams-tex-mode)
      :help "Run plain TeX")
     ("LaTeX" "%`%l%(mode)%' %t" TeX-run-TeX nil
      (latex-mode doctex-mode)
      :help "Run LaTeX")
     ("Makeinfo" "makeinfo %(extraopts) %t" TeX-run-compile nil
      (texinfo-mode)
      :help "Run Makeinfo with Info output")
     ("Makeinfo HTML" "makeinfo %(extraopts) --html %t" TeX-run-compile nil
      (texinfo-mode)
      :help "Run Makeinfo with HTML output")
     ("AmSTeX" "%(PDF)amstex %(extraopts) %`%S%(PDFout)%(mode)%' %t" TeX-run-TeX nil
      (ams-tex-mode)
      :help "Run AMSTeX")
     ("ConTeXt" "texexec --once --texutil %(extraopts) %(execopts)%t" TeX-run-TeX nil
      (context-mode)
      :help "Run ConTeXt once")
     ("ConTeXt Full" "texexec %(extraopts) %(execopts)%t" TeX-run-TeX nil
      (context-mode)
      :help "Run ConTeXt until completion")
     ("BibTeX" "bibtex %s" TeX-run-BibTeX nil t :help "Run BibTeX")
     ("Biber" "biber %s" TeX-run-Biber nil t :help "Run Biber")
     ("View" "%V" TeX-run-discard-or-function t t :help "Run Viewer")
     ("Print" "%p" TeX-run-command t t :help "Print the file")
     ("Queue" "%q" TeX-run-background nil t :help "View the printer queue" :visible TeX-queue-command)
     ("File" "%(o?)dvips %d -o %f " TeX-run-command t t :help "Generate PostScript file")
     ("Index" "makeindex %s" TeX-run-command nil t :help "Create index file")
     ("Xindy" "texindy %s" TeX-run-command nil t :help "Run xindy to create index file")
     ("Check" "lacheck %s" TeX-run-compile nil
      (latex-mode)
      :help "Check LaTeX file for correctness")
     ("ChkTeX" "chktex -v6 %s" TeX-run-compile nil
      (latex-mode)
      :help "Check LaTeX file for common mistakes")
     ("Spell" "(TeX-ispell-document \"\")" TeX-run-function nil t :help "Spell-check the document")
     ("Clean" "TeX-clean" TeX-run-function nil t :help "Delete generated intermediate files")
     ("Clean All" "(TeX-clean t)" TeX-run-function nil t :help "Delete generated intermediate and output files")
     ("latexmk" "latexmk -pdflatex='pdflatex -file-line-error -synctex=1' -pdf -interaction=nonstopmode %t" TeX-run-command t t :help "Run latexmk command"))))
 '(avy-background t)
 '(beacon-dont-blink-major-modes
   (quote
    (t magit-status-mode magit-popup-mode gnus-summary-mode gnus-group-mode term-mode)))
 '(custom-safe-themes
   (quote
    ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "f5eb916f6bd4e743206913e6f28051249de8ccfd070eae47b5bde31ee813d55f" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" default)))
 '(desktop-files-not-to-save "\\(^/[^/:]*:\\|(ftp)$\\|plstore\\)")
 '(diff-hl-fringe-bmp-function (quote storax/diff-hl-fringe-bmp-from-type))
 '(flymake-allowed-file-name-masks
   (quote
    (("\\.py\\'" elpy-flymake-python-init)
     ("\\.\\(?:c\\(?:pp\\|xx\\|\\+\\+\\)?\\|CC\\)\\'" flymake-simple-make-init)
     ("\\.xml\\'" flymake-xml-init)
     ("\\.html?\\'" flymake-xml-init)
     ("\\.cs\\'" flymake-simple-make-init)
     ("\\.p[ml]\\'" flymake-perl-init)
     ("\\.php[345]?\\'" flymake-php-init)
     ("\\.h\\'" flymake-master-make-header-init flymake-master-cleanup)
     ("\\.java\\'" flymake-simple-make-java-init flymake-simple-java-cleanup)
     ("\\.idl\\'" flymake-simple-make-init))))
 '(font-latex-fontify-sectioning (quote color))
 '(git-gutter:modified-sign "*")
 '(helm-boring-file-regexp-list
   (quote
    ("\\.o$" "~$" "\\.bin$" "\\.lbin$" "\\.so$" "\\.a$" "\\.ln$" "\\.blg$" "\\.bbl$" "\\.elc$" "\\.lof$" "\\.glo$" "\\.idx$" "\\.lot$" "\\.svn$" "\\.hg$" "\\.git$" "\\.bzr$" "CVS$" "_darcs$" "_MTN$" "\\.fmt$" "\\.tfm$" "\\.class$" "\\.fas$" "\\.lib$" "\\.mem$" "\\.x86f$" "\\.sparcf$" "\\.dfsl$" "\\.pfsl$" "\\.d64fsl$" "\\.p64fsl$" "\\.lx64fsl$" "\\.lx32fsl$" "\\.dx64fsl$" "\\.dx32fsl$" "\\.fx64fsl$" "\\.fx32fsl$" "\\.sx64fsl$" "\\.sx32fsl$" "\\.wx64fsl$" "\\.wx32fsl$" "\\.fasl$" "\\.ufsl$" "\\.fsl$" "\\.dxl$" "\\.lo$" "\\.la$" "\\.gmo$" "\\.mo$" "\\.toc$" "\\.aux$" "\\.cp$" "\\.fn$" "\\.ky$" "\\.pg$" "\\.tp$" "\\.vr$" "\\.cps$" "\\.fns$" "\\.kys$" "\\.pgs$" "\\.tps$" "\\.vrs$" "\\.pyc$" "\\.pyo$" "__pycache__")))
 '(helm-ff-skip-boring-files t)
 '(initial-frame-alist (quote ((fullscreen . maximized))))
 '(magit-auto-revert-mode nil)
 '(magit-push-arguments (quote ("--set-upstream")))
 '(minimap-recenter-type (quote free))
 '(minimap-window-location (quote right))
 '(org-agenda-files (quote ("~/Documents/org/Umzug.org")))
 '(shell-pop-default-directory nil)
 '(shell-pop-full-span t)
 '(shell-pop-shell-type (quote ("multi-term" "*zsh*" (lambda nil (multi-term)))))
 '(shell-pop-universal-key "<f10>")
 '(shell-pop-window-position "bottom")
 '(shell-pop-window-size 60)
 '(yas-fallback-behavior (quote call-other-command)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(avy-background-face ((t (:background "#3F3F3F" :foreground "#757565" :inverse-video nil))))
 '(avy-lead-face ((t (:background "#E8BF6A" :foreground "black" :inverse-video nil))))
 '(avy-lead-face-0 ((t (:background "#C45837" :foreground "white" :inverse-video nil))))
 '(avy-lead-face-1 ((t (:background "#6D9CBE" :foreground "black"))))
 '(avy-lead-face-2 ((t (:background "#B4C973" :foreground "black"))))
 '(diff-hl-change ((t (:background "#4F4F4F" :foreground "#FF88FF"))))
 '(diff-hl-delete ((t (:background "#4F4F4F" :foreground "#FF5353"))))
 '(diff-hl-insert ((t (:background "#4F4F4F" :foreground "#7FFF7F"))))
 '(font-latex-sectioning-5-face ((t (:inherit fixed-pitch :foreground "#CC9393" :weight bold))))
 '(git-gutter:modified ((t (:background "#DC8CC3" :foreground "#3F3F3F" :weight bold))))
 '(mode-line ((t (:background "#2B2B2B" :foreground "#8FB28F" :box nil :overline "dim gray"))))
 '(mode-line-inactive ((t (:background "#383838" :foreground "#5F7F5F" :box nil :overline "dim gray"))))
 '(powerline-inactive1 ((t (:inherit mode-line-inactive :background "#4F4F4F" :foreground "#7F9F7F"))))
 '(powerline-inactive2 ((t (:inherit mode-line-inactive :background "#6F6F6F" :foreground "#9FC29F"))))
 '(region ((t (:background "#222222"))))
 '(scroll-bar ((t (:background "#545450" :foreground "#3F3F3F")))))

(provide '.emacs-custom)
;;; .emacs-custom.el ends here
