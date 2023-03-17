;; -*- lexical-binding: t; -*-

;; minimalist look
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

;; time
(setenv "TZ" "UTC-8")
(customize-set-variable 'display-time-format "%l:%M %p %b %y")
(customize-set-variable 'display-time-default-load-average nil)

;; fonts
(defvar erasmo-ui--font-scale-factor erasmo-env-font-scale-factor)
(defvar erasmo-ui--font-size 142)

;; fix up the font setting function
(defun erasmo-ui-set-font-size (&optional SCALE-FACTOR)
  (interactive "nFont scale factor: ")
  (if SCALE-FACTOR
      (setq erasmo-ui--font-scale-factor SCALE-FACTOR))
  (erasmo-ui-set-font))

(defun erasmo-ui-set-font ()
  ;; default font face
  (set-face-attribute 'default nil
                      :font "JetBrains Mono"
                      :height (ceiling (* erasmo-ui--font-size erasmo-ui--font-scale-factor)))

  ;; Set the fixed pitch face
  (set-face-attribute 'fixed-pitch nil
                      :font "Fira Code Retina"
                      :height (ceiling (* erasmo-ui--font-size erasmo-ui--font-scale-factor)))

  ;; Set the variable pitch face
  (set-face-attribute 'variable-pitch nil
                      :font "Iosevka Aile"
                      :height (ceiling (* erasmo-ui--font-size erasmo-ui--font-scale-factor))))

(erasmo-ui-set-font)

;; pretty icons
(use-package all-the-icons
  :custom
  (all-the-icons-scale-factor 1))
(use-package all-the-icons-dired
  :init
  (all-the-icons-dired-mode))

;; for completion buffers
(use-package all-the-icons-completion
  :init
  (all-the-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'all-the-icons-completion-marginalia-setup))

;; Themes
;; A nice gallery of Emacs themes can be found at https://emacsthemes.com/.

(use-package spacegray-theme :defer t)

(use-package doom-themes
  :defer t
  :config
  (doom-themes-visual-bell-config)
  )

(use-package darktooth-theme :defer t)

(use-package ample-theme
  :init (progn (load-theme 'ample t t)
               (load-theme 'ample-flat t t)
               (load-theme 'ample-light t t))
  :defer t)

(use-package autumn-light-theme :defer t)

(defun erasmo-ui--load-theme (theme)
  "Enhance `load-theme' by first disabling enabled themes."
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme theme t)
  (if (string-equal major-mode "org-mode")
      (progn
        (erasmo-org-mode-setup)
        (erasmo-org-set-org-agenda-files)))
  )
(erasmo-ui--load-theme 'doom-zenburn)

;; theme toggle
(defhydra erasmo-ui-hydra-theme-switcher (:hint nil)
  "
         Dark                ^Light^
    ----------------------------------------------
    _1_ zenburn          _w_ flatwhite
    _2_ ample            _e_ autumn
    _3_ darktooth       ^
    _4_ snazzy           ^
    _5_ old-hope         ^
    _6_ henna                ^
    _7_ palenight            ^
    _8_ peacock              ^
    _q_ quit                 ^
    ^                        ^
    "
  ;; Dark
  ("1" (erasmo-ui--load-theme 'doom-zenburn) "zenburn")
  ("2" (erasmo-ui--load-theme 'ample-flat) "ample")
  ("3" (erasmo-ui--load-theme 'darktooth-dark) "darktooth")
  ("4" (erasmo-ui--load-theme 'doom-snazzy) "snazzy")
  ("5" (erasmo-ui--load-theme 'doom-old-hope) "old-hope")
  ("6" (erasmo-ui--load-theme 'doom-henna) "henna")
  ("7" (erasmo-ui--load-theme 'doom-palenight) "doom-palenight")
  ("8" (erasmo-ui--load-theme 'doom-peacock) "peacock")

  ;; Light
  ("w" (erasmo-ui--load-theme 'doom-flatwhite) "flatwhite")
  ("e" (erasmo-ui--load-theme 'autumn-light) "autumn")
  ("r" (erasmo-ui--load-theme 'marquardt) "marquardt")

  ("q" nil))

(erasmo-keybind-leader-key-def
 "tm" '(erasmo-ui-hydra-theme-switcher/body :which-key "choose theme"))

;; shorten package names
;; According to [[https://www.emacswiki.org/emacs/DiminishedModes][Emacs Wiki]], the ~:diminish~ flag in ~use-package~ does not work on mode lines that use ~:eval~ forms. So we have to insert a small hack after each of these packages. Below is an example code, but also needed for ~buffer-face-mode~
(use-package diminish
  :init
  (eval-after-load "face-remap" '(diminish 'buffer-face-mode)))

;; pretty modeline
(use-package doom-modeline
  :config
  (custom-set-faces '(mode-line ((t (:height 0.85))))
                    '(mode-line-inactive ((t (:height 0.85)))))
  :custom
  (doom-modeline-height 15)
  (doom-modeline-bar-width 6)
  (doom-modeline-lsp t)
  (doom-modeline-github nil)
  (doom-modeline-mu4e nil)
  (doom-modeline-irc nil)
  (doom-modeline-minor-modes t)
  (doom-modeline-persp-name nil)
  (doom-modeline-buffer-file-name-style 'truncate-except-project)
  (doom-modeline-major-mode-icon t)
  (doom-modeline-time t)
  (doom-modeline-status t))

(defun erasmo-ui--start-doom-modeline ()
  (require 'doom-modeline)
  ;; Start it
  (doom-modeline-mode 1)
  ;; Customize the default modeline
  (doom-modeline-def-modeline 'default
    '(bar window-number modals matches buffer-info remote-host buffer-position word-count parrot selection-info)
    '(objed-state grip debug repl lsp minor-modes input-method indent-info buffer-encoding major-mode process vcs checker))
  (doom-modeline-set-modeline 'default t))

(add-hook 'after-init-hook #'erasmo-ui--start-doom-modeline)

;; rainbow dired
(use-package dired-rainbow
  :defer 2
  :config
  (dired-rainbow-define-chmod directory "#6cb2eb" "d.*")
  (dired-rainbow-define html "#eb5286" ("css" "less" "sass" "scss" "htm" "html" "jhtm" "mht" "eml" "mustache" "xhtml"))
  (dired-rainbow-define xml "#f2d024" ("xml" "xsd" "xsl" "xslt" "wsdl" "bib" "json" "msg" "pgn" "rss" "yaml" "yml" "rdata"))
  (dired-rainbow-define document "#9561e2" ("docm" "doc" "docx" "odb" "odt" "pdb" "pdf" "ps" "rtf" "djvu" "epub" "odp" "ppt" "pptx"))
  (dired-rainbow-define markdown "#ffed4a" ("org" "etx" "info" "markdown" "md" "mkd" "nfo" "pod" "rst" "tex" "textfile" "txt"))
  (dired-rainbow-define database "#6574cd" ("xlsx" "xls" "csv" "accdb" "db" "mdb" "sqlite" "nc"))
  (dired-rainbow-define media "#de751f" ("mp3" "mp4" "mkv" "MP3" "MP4" "avi" "mpeg" "mpg" "flv" "ogg" "mov" "mid" "midi" "wav" "aiff" "flac"))
  (dired-rainbow-define image "#f66d9b" ("tiff" "tif" "cdr" "gif" "ico" "jpeg" "jpg" "png" "psd" "eps" "svg"))
  (dired-rainbow-define log "#c17d11" ("log"))
  (dired-rainbow-define shell "#f6993f" ("awk" "bash" "bat" "sed" "sh" "zsh" "vim"))
  (dired-rainbow-define interpreted "#38c172" ("py" "ipynb" "rb" "pl" "t" "msql" "mysql" "pgsql" "sql" "r" "clj" "cljs" "scala" "js"))
  (dired-rainbow-define compiled "#4dc0b5" ("asm" "cl" "lisp" "el" "c" "h" "c++" "h++" "hpp" "hxx" "m" "cc" "cs" "cp" "cpp" "go" "f" "for" "ftn" "f90" "f95" "f03" "f08" "s" "rs" "hi" "hs" "pyc" ".java"))
  (dired-rainbow-define executable "#8cc4ff" ("exe" "msi"))
  (dired-rainbow-define compressed "#51d88a" ("7z" "zip" "bz2" "tgz" "txz" "gz" "xz" "z" "Z" "jar" "war" "ear" "rar" "sar" "xpi" "apk" "xz" "tar"))
  (dired-rainbow-define packaged "#faad63" ("deb" "rpm" "apk" "jad" "jar" "cab" "pak" "pk3" "vdf" "vpk" "bsp"))
  (dired-rainbow-define encrypted "#ffed4a" ("gpg" "pgp" "asc" "bfe" "enc" "signature" "sig" "p12" "pem"))
  (dired-rainbow-define fonts "#6cb2eb" ("afm" "fon" "fnt" "pfb" "pfm" "ttf" "otf"))
  (dired-rainbow-define partition "#e3342f" ("dmg" "iso" "bin" "nrg" "qcow" "toast" "vcd" "vmdk" "bak"))
  (dired-rainbow-define vc "#0074d9" ("git" "gitignore" "gitattributes" "gitmodules"))
  (dired-rainbow-define-chmod executable-unix "#38c172" "-.*x.*"))

(provide 'erasmo-ui)
