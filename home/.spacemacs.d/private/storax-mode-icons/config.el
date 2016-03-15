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

(defcustom font-icons
  '(("line_number" #xe0a1)
    ("column_number" #xe0a3)
    ("lock" #xe0a2)
    ("pl_l_flame_b" #xe0c0)
    ("pl_l_flame_w" #xe0c1)
    ("pl_r_flame_b" #xe0c2)
    ("pl_r_flame_w" #xe0c3)
    ("pl_l_pixels" #xe0c4)
    ("pl_r_pixels" #xe0c5)
    ("pl_l_pixels_big" #xe0c6)
    ("pl_r_pixels_big" #xe0c7)
    ("package" #xf0c4)
    ("android" #xe70e)
    ("angular" #xe753)
    ("appcelerator" #xe7ab)
    ("apple" #xe711)
    ("appstore" #xe713)
    ("aptana" #xe799)
    ("asterisk" #xe7ac)
    ("atlassian" #xe75b)
    ("atom" #xe764)
    ("aws" #xe7ad)
    ("backbone" #xe752)
    ("bing_small" #xe700)
    ("bintray" #xe794)
    ("bitbucket" #xe703)
    ("blackberry" #xe723)
    ("bootstrap" #xe747)
    ("bower" #xe74d)
    ("brackets" #xe79d)
    ("bugsense" #xe78d)
    ("celluloid" #xe76b)
    ("chrome" #xe743)
    ("cisco" #xe765)
    ("clojure" #xe768)
    ("clojure_alt" #xe76a)
    ("cloud9" #xe79f)
    ("coda" #xe793)
    ("code" #xe796)
    ("code_badge" #xe7a3)
    ("codeigniter" #xe780)
    ("codepen" #xe716)
    ("codrops" #xe72f)
    ("coffeescript" #xe751)
    ("compass" #xe761p)
    ("composer" #xe783)
    ("creativecommons" #xe789)
    ("creativecommons_badge" #xe78a)
    ("css3" #xe749)
    ("css3_full" #xe74a)
    ("css_tricks" #x701)
    ("cssdeck" #xe72a)
    ("dart" #xe798)
    ("database" #xe706)
    ("debian" #xe77d)
    ("digital-ocean" #xe7ae)
    ("django" #xe71d)
    ("dlang" #xe7af)
    ("docker" #xe7b0)
    ("doctrine" #xe774)
    ("dojo" #xe71c)
    ("dotnet" #xe77f)
    ("dreamweaver" #xe79c)
    ("dropbox" #xe707)
    ("drupal" #xe742)
    ("eclipse" #xe79e)
    ("ember" #xe71b)
    ("envato" #xe75d)
    ("erlang" #xe7b1)
    ("extjs" #xe78e)
    ("firebase" #xe787)
    ("firefox" #xe745)
    ("fsharp" #xe7a7)
    ("ghost" #xe71f)
    ("ghost_small" #xe714)
    ("git" #xe702)
    ("git_branch" #xe725)
    ("git_commit" #xe729)
    ("git_compare" #xe728)
    ("git_merge" #xe727)
    ("git_pull_request" #xe726)
    ("github" #xe709)
    ("github_alt" #xe708)
    ("github_badge" #xe70a)
    ("github_full" #xe717)
    ("gnu" #xe779)
    ("go" #xe724)
    ("google-cloud-platform" #xe7b2)
    ("google_drive" #xe731)
    ("grails" #xe7b3)
    ("groovy" #xe775)
    ("grunt" #xe74c)
    ("gulp" #xe763)
    ("hackernews" #xe71a)
    ("haskell" #xe777)
    ("heroku" #xe77b)
    ("html5" #xe736)
    ("html5_3d_effects" #xe735)
    ("html5_connectivity" #xe734)
    ("html5_device_access" #xe733)
    ("html5_multimedia" #xe732)
    ("ie" #xe744)
    ("illustrator" #xe7b4)
    ("intellij" #xe7b5)
    ("ionic" #xe7a9)
    ("java" #xe738)
    ("javascript" #xe74e)
    ("javascript_badge" #xe781)
    ("javascript_shield" #xe74f)
    ("jekyll_small" #xe70d)
    ("jenkins" #xe767)
    ("jira" #xe75c)
    ("joomla" #xe741)
    ("jquery" #xe750)
    ("jquery_ui" #xe754)
    ("komodo" #xe792)
    ("krakenjs" #xe785)
    ("krakenjs_badge" #xe784)
    ("laravel" #xe73f)
    ("less" #xe758)
    ("linux" #xe712)
    ("magento" #xe740)
    ("mailchimp" #xe79a)
    ("markdown" #xe73e)
    ("materializecss" #xe7b6)
    ("meteor" #xe7a5)
    ("meteorfull" #xe7a6)
    ("mitlicence" #xe78b)
    ("modernizr" #xe720)
    ("mongodb" #xe7a4)
    ("mootools" #xe790)
    ("mootools_badge" #xe78f)
    ("mozilla" #xe786)
    ("msql_server" #xe77c)
    ("mysql" #xe704)
    ("nancy" #xe766)
    ("netbeans" #xe79b)
    ("netmagazine" #xe72e)
    ("nginx" #xe776)
    ("nodejs" #xe719)
    ("nodejs_small" #xe718)
    ("npm" #xe71e)
    ("onedrive" #xe762)
    ("openshift" #xe7b7)
    ("opensource" #xe771)
    ("opera" #xe746)
    ("perl" #xe769)
    ("phonegap" #xe730)
    ("photoshop" #xe7b8)
    ("php" #xe73d)
    ("postgresql" #xe76e)
    ("prolog" #xe7a1)
    ("python" #xe73c)
    ("rackspace" #xe7b9)
    ("raphael" #xe75f)
    ("rasberry_pi" #xe722)
    ("react" #xe7ba)
    ("redhat" #xe7bb)
    ("redis" #xe76d)
    ("requirejs" #xe770)
    ("responsive" #xe797)
    ("ruby" #xe739)
    ("ruby_on_rails" #xe73b)
    ("ruby_rough" #xe791)
    ("rust" #xe7a8)
    ("safari" #xe748)
    ("sass" #xe74b)
    ("scala" #xe737)
    ("scriptcs" #xe7bc)
    ("scrum" #xe7a0)
    ("senchatouch" #xe78c)
    ("sizzlejs" #xe788)
    ("smashing_magazine" #xe72d)
    ("snap_svg" #xe75e)
    ("sqllite" #xe7c4)
    ("stackoverflow" #xe710)
    ("streamline" #xe705)
    ("stylus" #xe759)
    ("sublime" #xe7aa)
    ("swift" #xe755)
    ("symfony" #xe756)
    ("symfony_badge" #xe757)
    ("techcrunch" #xe72c)
    ("terminal" #xe795)
    ("terminal_badge" #xe7a2)
    ("travis" #xe77e)
    ("trello" #xe75a)
    ("typo3" #xe772)
    ("ubuntu" #xe73a)
    ("uikit" #xe773)
    ("unity_small" #xe721)
    ("vim" #xe7c5)
    ("visualstudio" #xe70c)
    ("w3c" #xe76c)
    ("webplatform" #xe76f)
    ("windows" #xe70f)
    ("wordpress" #xe70b)
    ("yahoo" #xe715)
    ("yahoo_small" #xe72b)
    ("yeoman" #xe77a)
    ("yii" #xe782)
    ("zend" #xe778))
  "Font Icons."
  :type '(alist :value-type (group character))
  :group 'mode-icons)

(mode-icons-mode)

;;; config.el ends here
