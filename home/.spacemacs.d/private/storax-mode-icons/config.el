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
                (choice
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

(defcustom font-icons
  '(("line_number" (char-to string #xe0a1))
    ("column_number" (char-to-string #xe0a3))
    ("lock" (char-to-string #xe0a2))
    ("pl_l_flame_b" (char-to-string #xe0c0))
    ("pl_l_flame_w" (char-to-string #xe0c1))
    ("pl_r_flame_b" (char-to-string #xe0c2))
    ("pl_r_flame_w" (char-to-string #xe0c3))
    ("pl_l_pixels" (char-to-string #xe0c4))
    ("pl_r_pixels" (char-to-string #xe0c5))
    ("pl_l_pixels_big" (char-to-string #xe0c6))
    ("pl_r_pixels_big" (char-to-string #xe0c7))
    ("package" (char-to-string #xf0c4))
    ("android" (char-to-string #xe70e))
    ("angular" (char-to-string #xe753))
    ("appcelerator" (char-to-string #xe7ab))
    ("apple" (char-to-string #xe711))
    ("appstore" (char-to-string #xe713))
    ("aptana" (char-to-string #xe799))
    ("asterisk" (char-to-string #xe7ac))
    ("atlassian" (char-to-string #xe75b))
    ("atom" (char-to-string #xe764))
    ("aws" (char-to-string #xe7ad))
    ("backbone" (char-to-string #xe752))
    ("bing_small" (char-to-string #xe700))
    ("bintray" (char-to-string #xe794))
    ("bitbucket" (char-to-string #xe703))
    ("blackberry" (char-to-string #xe723))
    ("bootstrap" (char-to-string #xe747))
    ("bower" (char-to-string #xe74d))
    ("brackets" (char-to-string #xe79d))
    ("bugsense" (char-to-string #xe78d))
    ("celluloid" (char-to-string #xe76b))
    ("chrome" (char-to-string #xe743))
    ("cisco" (char-to-string #xe765))
    ("clojure" (char-to-string #xe768))
    ("clojure_alt" (char-to-string #xe76a))
    ("cloud9" (char-to-string #xe79f))
    ("coda" (char-to-string #xe793))
    ("code" (char-to-string #xe796))
    ("code_badge" (char-to-string #xe7a3))
    ("codeigniter" (char-to-string #xe780))
    ("codepen" (char-to-string #xe716))
    ("codrops" (char-to-string #xe72f))
    ("coffeescript" (char-to-string #xe751))
    ("compass" (char-to-string #xe761))
    ("composer" (char-to-string #xe783))
    ("creativecommons" (char-to-string #xe789))
    ("creativecommons_badge" (char-to-string #xe78a))
    ("css3" (char-to-string #xe749))
    ("css3_full" (char-to-string #xe74a))
    ("css_tricks" (char-to-string #x701))
    ("cssdeck" (char-to-string #xe72a))
    ("dart" (char-to-string #xe798))
    ("database" (char-to-string #xe706))
    ("debian" (char-to-string #xe77d))
    ("digital-ocean" (char-to-string #xe7ae))
    ("django" (char-to-string #xe71d))
    ("dlang" (char-to-string #xe7af))
    ("docker" (char-to-string #xe7b0))
    ("doctrine" (char-to-string #xe774))
    ("dojo" (char-to-string #xe71c))
    ("dotnet" (char-to-string #xe77f))
    ("dreamweaver" (char-to-string #xe79c))
    ("dropbox" (char-to-string #xe707))
    ("drupal" (char-to-string #xe742))
    ("eclipse" (char-to-string #xe79e))
    ("ember" (char-to-string #xe71b))
    ("envato" (char-to-string #xe75d))
    ("erlang" (char-to-string #xe7b1))
    ("extjs" (char-to-string #xe78e))
    ("firebase" (char-to-string #xe787))
    ("firefox" (char-to-string #xe745))
    ("fsharp" (char-to-string #xe7a7))
    ("ghost" (char-to-string #xe71f))
    ("ghost_small" (char-to-string #xe714))
    ("git" (char-to-string #xe702))
    ("git_branch" (char-to-string #xe725))
    ("git_commit" (char-to-string #xe729))
    ("git_compare" (char-to-string #xe728))
    ("git_merge" (char-to-string #xe727))
    ("git_pull_request" (char-to-string #xe726))
    ("github" (char-to-string #xe709))
    ("github_alt" (char-to-string #xe708))
    ("github_badge" (char-to-string #xe70a))
    ("github_full" (char-to-string #xe717))
    ("gnu" (char-to-string #xe779))
    ("go" (char-to-string #xe724))
    ("google-cloud-platform" (char-to-string #xe7b2))
    ("google_drive" (char-to-string #xe731))
    ("grails" (char-to-string #xe7b3))
    ("groovy" (char-to-string #xe775))
    ("grunt" (char-to-string #xe74c))
    ("gulp" (char-to-string #xe763))
    ("hackernews" (char-to-string #xe71a))
    ("haskell" (char-to-string #xe777))
    ("heroku" (char-to-string #xe77b))
    ("html5" (char-to-string #xe736))
    ("html5_3d_effects" (char-to-string #xe735))
    ("html5_connectivity" (char-to-string #xe734))
    ("html5_device_access" (char-to-string #xe733))
    ("html5_multimedia" (char-to-string #xe732))
    ("ie" (char-to-string #xe744))
    ("illustrator" (char-to-string #xe7b4))
    ("intellij" (char-to-string #xe7b5))
    ("ionic" (char-to-string #xe7a9))
    ("java" (char-to-string #xe738))
    ("javascript" (char-to-string #xe74e))
    ("javascript_badge" (char-to-string #xe781))
    ("javascript_shield" (char-to-string #xe74f))
    ("jekyll_small" (char-to-string #xe70d))
    ("jenkins" (char-to-string #xe767))
    ("jira" (char-to-string #xe75c))
    ("joomla" (char-to-string #xe741))
    ("jquery" (char-to-string #xe750))
    ("jquery_ui" (char-to-string #xe754))
    ("komodo" (char-to-string #xe792))
    ("krakenjs" (char-to-string #xe785))
    ("krakenjs_badge" (char-to-string #xe784))
    ("laravel" (char-to-string #xe73f))
    ("less" (char-to-string #xe758))
    ("linux" (char-to-string #xe712))
    ("magento" (char-to-string #xe740))
    ("mailchimp" (char-to-string #xe79a))
    ("markdown" (char-to-string #xe73e))
    ("materializecss" (char-to-string #xe7b6))
    ("meteor" (char-to-string #xe7a5))
    ("meteorfull" (char-to-string #xe7a6))
    ("mitlicence" (char-to-string #xe78b))
    ("modernizr" (char-to-string #xe720))
    ("mongodb" (char-to-string #xe7a4))
    ("mootools" (char-to-string #xe790))
    ("mootools_badge" (char-to-string #xe78f))
    ("mozilla" (char-to-string #xe786))
    ("msql_server" (char-to-string #xe77c))
    ("mysql" (char-to-string #xe704))
    ("nancy" (char-to-string #xe766))
    ("netbeans" (char-to-string #xe79b))
    ("netmagazine" (char-to-string #xe72e))
    ("nginx" (char-to-string #xe776))
    ("nodejs" (char-to-string #xe719))
    ("nodejs_small" (char-to-string #xe718))
    ("npm" (char-to-string #xe71e))
    ("onedrive" (char-to-string #xe762))
    ("openshift" (char-to-string #xe7b7))
    ("opensource" (char-to-string #xe771))
    ("opera" (char-to-string #xe746))
    ("perl" (char-to-string #xe769))
    ("phonegap" (char-to-string #xe730))
    ("photoshop" (char-to-string #xe7b8))
    ("php" (char-to-string #xe73d))
    ("postgresql" (char-to-string #xe76e))
    ("prolog" (char-to-string #xe7a1))
    ("python" (char-to-string #xe73c))
    ("rackspace" (char-to-string #xe7b9))
    ("raphael" (char-to-string #xe75f))
    ("rasberry_pi" (char-to-string #xe722))
    ("react" (char-to-string #xe7ba))
    ("redhat" (char-to-string #xe7bb))
    ("redis" (char-to-string #xe76d))
    ("requirejs" (char-to-string #xe770))
    ("responsive" (char-to-string #xe797))
    ("ruby" (char-to-string #xe739))
    ("ruby_on_rails" (char-to-string #xe73b))
    ("ruby_rough" (char-to-string #xe791))
    ("rust" (char-to-string #xe7a8))
    ("safari" (char-to-string #xe748))
    ("sass" (char-to-string #xe74b))
    ("scala" (char-to-string #xe737))
    ("scriptcs" (char-to-string #xe7bc))
    ("scrum" (char-to-string #xe7a0))
    ("senchatouch" (char-to-string #xe78c))
    ("sizzlejs" (char-to-string #xe788))
    ("smashing_magazine" (char-to-string #xe72d))
    ("snap_svg" (char-to-string #xe75e))
    ("sqllite" (char-to-string #xe7c4))
    ("stackoverflow" (char-to-string #xe710))
    ("streamline" (char-to-string #xe705))
    ("stylus" (char-to-string #xe759))
    ("sublime" (char-to-string #xe7aa))
    ("swift" (char-to-string #xe755))
    ("symfony" (char-to-string #xe756))
    ("symfony_badge" (char-to-string #xe757))
    ("techcrunch" (char-to-string #xe72c))
    ("terminal" (char-to-string #xe795))
    ("terminal_badge" (char-to-string #xe7a2))
    ("travis" (char-to-string #xe77e))
    ("trello" (char-to-string #xe75a))
    ("typo3" (char-to-string #xe772))
    ("ubuntu" (char-to-string #xe73a))
    ("uikit" (char-to-string #xe773))
    ("unity_small" (char-to-string #xe721))
    ("vim" (char-to-string #xe7c5))
    ("visualstudio" (char-to-string #xe70c))
    ("w3c" (char-to-string #xe76c))
    ("webplatform" (char-to-string #xe76f))
    ("windows" (char-to-string #xe70f))
    ("wordpress" (char-to-string #xe70b))
    ("yahoo" (char-to-string #xe715))
    ("yahoo_small" (char-to-string #xe72b))
    ("yeoman" (char-to-string #xe77a))
    ("yii" (char-to-string #xe782))
    ("zend" (char-to-string #xe778)))
  "Font Icons."
  :type '(alist :value-type (group character))
  :group 'mode-icons)

(mode-icons-mode)

;;; config.el ends here
