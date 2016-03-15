;;; config.el --- storax-powerline layer config file for Spacemacs.
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

(mode-icons-define-font "github-octicons")
(mode-icons-define-font "font-mfizz")
(mode-icons-define-font "FontAwesome")
(mode-icons-define-font "IcoMoon-Free")
(mode-icons-define-font "icomoon")
(mode-icons-define-font "Sauce Code Pro")

(defcustom mode-icons
  `((" Golden" "golden" xpm) ;; Icon created by Arthur Shlain from Noun Project
    (" Pulls" #xf092 FontAwesome)
    (" Rbow" "rainbow" xpm)
    (" hs" "hs" xpm)
    (" yas" "yas" xpm)
    ("AHK" "autohotkey" xpm)
    ("ARev" #xf021 FontAwesome)
    ("BibTeX" "bibtex" xpm)
    ("C/l" #xf107 font-mfizz)
    ("C?Perl" #xf148 font-mfizz)
    ("CSS" "css" xpm)
    ("C[#]/l" #xf10d font-mfizz)
    ("C[+][+]/l" #xf10c font-mfizz)
    ("Calc\\(ulator\\)?" #xf1ec FontAwesome)
    ("Calendar" #xf073 FontAwesome)
    ("Clojure" #xf10a font-mfizz)
    ("Coffee" "coffee" xpm)
    ("Compilation" "compile" xpm)
    ("Custom" #xf013 FontAwesome)
    ("Debug.*" #xf188 FontAwesome)
    ("Debug.*" #xf188 FontAwesome)
    ("ESS\\[BUGS\\]" #xf188 FontAwesome)
    ("ESS\\[SAS\\]" "sas" xpm)
    ("ESS\\[S\\]" "R" xpm)
    ("Elixir" #xf115 font-mfizz)
    ("Emacs-Lisp" "emacs" xpm)
    ("Erlang" #xf116 font-mfizz)
    ("HTML" "html" xpm)
    ("Haml" "haml" xpm)
    ("Haskell" #xf126 font-mfizz)
    ("Help" #xf059 FontAwesome)
    ("Image[imagemagick]" "svg" xpm)
    ("Inf-Ruby" "infruby" xpm)
    ("Info" #xf05a FontAwesome)
    ("Java/l" #xf12b font-mfizz)
    ("JavaScript" "js" xpm)
    ("Lisp Interaction" "emacs" xpm)
    ("Lisp" "cl" xpm)
    ("Magit" #xf1d2 FontAwesome)
    ("Markdown" #xf0c9 github-octicons)
    ("Narrow" #xf066 FontAwesome)
    ("Octave" "octave" xpm)
    ("Org" "org" xpm)
    ("PHP" "php" xpm)
    ("PHP/l" "php" xpm)
    ("Projectile Rails Server" "rails" xpm)
    ("Python" "python" xpm)
    ("Ruby" "ruby" xpm)
    ("SCSS" "sass" xpm)
    ("Sass" "sass" xpm)
    ("Scala" #xf15b font-mfizz)
    ("Scheme" "scheme" xpm)
    ("Shell-script" "bash" xpm)
    ("Slim" "slim" xpm)
    ("Snippet" "yas" xpm)
    ("Term" "term" xpm)
    ("Text\\'" #xf0f6 FontAwesome)
    ("Web" "html" xpm)
    ("WoMan" #xf05a FontAwesome)
    ("XML" "xml" xpm)
    ("YAML" "yaml" xpm)
    ("YASnippet" "yas" xpm)
    ("Zip-Archive" #xf1c6 FontAwesome)
    ("\\` ?company\\'" #xf1ad FontAwesome)
    ("\\`Go\\'" "go" xpm)
    ("iESS" "R" xpm)
    ("nXML" "xml" xpm)

    (apple #xf179 FontAwesome)
    (modified-outside #xf071 FontAwesome)
    (read-only #xf023 FontAwesome)
    (save #xf0c7 FontAwesome)
    (saved "" nil)
    (steal #xf21b FontAwesome)
    (undecided #xf128 FontAwesome)
    (unix #xeabd IcoMoon-Free)  ;; Clear Tux (Unlike FontAwesome)
    (unix #xf166 font-mfizz)    ;; Use ubuntu, since I think it is the most common.
    (unix #xf17c FontAwesome) ;; Fall Back to FontAwesome
    (win #xf17a FontAwesome)
    (writable #xf09c FontAwesome)
    ;; FIXME: use lsb_release to determine Linux variant and choose appropriate icon
    ;; This icon is clearer than FontAwesome's Linux Penguin
    ;; Diminished modes
    ("\\(?:ElDoc\\|Anzu\\|SP\\|Guide\\|PgLn\\|Undo-Tree\\|Ergo.*\\|,\\|Isearch\\|Ind\\|Fly\\)" nil nil))
  "Icons for majoppr and minor modes.

Each specificatioun is a list with the first element being the
name of the major mode.  The second the name of the icon file,
without the extension.  And the third being the type of icon."
  :type '(repeat
          (list (choice
                 (string :tag "Regular Expression")
                 (const :tag "Read Only Indicator" read-only)
                 (const :tag "Writable Indicator" writable)
                 (const :tag "Saved" saved)
                 (const :tag "Save" save)
                 (const :tag "Modified Outside Emacs" modified-outside)
                 (const :tag "Locked By Someone Else" steal)
                 (const :tag "Apple" apple)
                 (const :tag "Windows" win)
                 (const :tag "Unix" unix))
p                (choice
                 (string :tag "Icon Name")
                 (integer :tag "Font Glyph Code")
                 (const :tag "Suppress" nil))
                (choice
                 (const :tag "text" nil)
                 (const :tag "Octicons" github-octicons)
                 (const :tag "Fizzed" font-mfizz)
                 (const :tag "Font Awesome" FontAwesome)
                 (const :tag "Ico Moon Free" IcoMoon-Free)
                 (const :tag "png" png)
                 (const :tag "gif" gif)
                 (const :tag "jpeg" jpeg)
                 (const :tag "jpg" jpg)
                 (const :tag "xbm" xbm)
                 (const :tag "xpm" xpm))))
  :group 'mode-icons)

(mode-icons-mode)

;;; config.el ends here
