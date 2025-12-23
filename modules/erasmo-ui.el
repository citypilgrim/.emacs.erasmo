;; -*- lexical-binding: t; -*-

;; minimalist look
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

;; add visual pulse when changing focus, like beacon but built-in
;; from from https://karthinks.com/software/batteries-included-with-emacs/
(defun pulse-line (&rest _)
  "Pulse the current line."
  (pulse-momentary-highlight-one-line (point)))

(dolist (command '(scroll-up-command scroll-down-command
                                     recenter-top-bottom other-window))
  (advice-add command :after #'pulse-line))

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
  (set-face-attribute 'Info-quoted nil
                      :font "Fira Code Retina"
                      :height (ceiling (* erasmo-ui--font-size erasmo-ui--font-scale-factor)))

  ;; Set the variable pitch face
  (set-face-attribute 'variable-pitch nil
                      :font "Iosevka Aile"
                      :height (ceiling (* erasmo-ui--font-size erasmo-ui--font-scale-factor))))

(add-hook 'after-init-hook #'erasmo-ui-set-font)

;; emoji support
(use-package emojify
  :hook (after-init . global-emojify-mode)
  :custom
  (emojify-download-emojis-p nil))

;; pretty icons
(use-package all-the-icons
  :custom
  (all-the-icons-scale-factor 1))

(use-package all-the-icons-dired
  :init
  (add-hook 'dired-mode-hook
            (lambda ()
              (interactive)
              (unless (s-equals? "/gnu/store/" (expand-file-name default-directory))
                (all-the-icons-dired-mode 1)
                (diminish 'all-the-icons-dired-mode)))))

;; for completion buffers
(use-package all-the-icons-completion
  :init
  (all-the-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'all-the-icons-completion-marginalia-setup))

;; for ibuffer
(use-package all-the-icons-ibuffer
  :hook (ibuffer-mode . all-the-icons-ibuffer-mode))

;; Themes
;; A nice gallery of Emacs themes can be found at https://emacsthemes.com/.
;; Themes used have to be acceptive of the font faces defined in

(use-package doom-themes
  :defer t
  :config
  (doom-themes-visual-bell-config)
  )

(use-package ef-themes
  :defer t)

(defun erasmo-ui--init-theme (theme)
  "Enhance `load-theme' by first disabling enabled themes."
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme theme t))

(defun erasmo-ui--load-theme (theme)
  "Configures required settings after theme is loaded."
  (erasmo-ui--init-theme theme)
  (if (string-equal major-mode "org-mode")
      (progn
        (erasmo-org--mode-setup)
        (erasmo-org-set-org-agenda-files))))

;; theme toggle
(defhydra erasmo-ui-hydra-theme-switcher (:hint nil)
  "
         Dark                ^Light^
    ----------------------------------------------
    _1_ zenburn          _w_ tomorrow
    _2_ palenight        _e_ duo
    _3_ oksolar          _r_ bluloco
    _4_ bluloco          _t_ tomorrow
    _5_ tomorrow         ^
    _q_ quit             ^
    ^                    ^
    "
  ;; Dark
  ("1" (erasmo-ui--load-theme 'doom-zenburn) "zenburn")
  ("2" (erasmo-ui--load-theme 'doom-palenight) "palenight")
  ("3" (erasmo-ui--load-theme 'doom-oksolar-dark) "oksolar")
  ("4" (erasmo-ui--load-theme 'doom-bluloco-dark) "bluloco")
  ("5" (erasmo-ui--load-theme 'doom-tomorrow-night) "tomorrow")

  ;; Light
  ("w" (erasmo-ui--load-theme 'doom-solarized-light) "solarized")
  ("e" (erasmo-ui--load-theme 'doom-oksolar-light) "oksolar")
  ("r" (erasmo-ui--load-theme 'doom-bluloco-light) "bluloco")
  ("t" (erasmo-ui--load-theme 'doom-tomorrow-day) "tomorrow")

  ("q" nil))

(erasmo-keybind-leader-key-def
 "tm" '(erasmo-ui-hydra-theme-switcher/body :which-key "choose theme"))

;; pretty modeline
(use-package doom-modeline
  :config
  (custom-set-faces '(mode-line ((t (:height 0.85))))
                    '(mode-line-inactive ((t (:height 0.85)))))
  :custom
  ;; If non-nil, cause imenu to see `doom-modeline' declarations.
  ;; This is done by adjusting `lisp-imenu-generic-expression' to
  ;; include support for finding `doom-modeline-def-*' forms.
  ;; Must be set before loading doom-modeline.
  (doom-modeline-support-imenu t)

  ;; How tall the mode-line should be. It's only respected in GUI.
  ;; If the actual char height is larger, it respects the actual height.
  (doom-modeline-height 25)

  ;; How wide the mode-line bar should be. It's only respected in GUI.
  (doom-modeline-bar-width 4)

  ;; Whether to use hud instead of default bar. It's only respected in GUI.
  (doom-modeline-hud nil)

  ;; The limit of the window width.
  ;; If `window-width' is smaller than the limit, some information won't be
  ;; displayed. It can be an integer or a float number. `nil' means no limit."
  (doom-modeline-window-width-limit 85)

  ;; How to detect the project root.
  ;; nil means to use `default-directory'.
  ;; The project management packages have some issues on detecting project root.
  ;; e.g. `projectile' doesn't handle symlink folders well, while `project' is unable
  ;; to hanle sub-projects.
  ;; You can specify one if you encounter the issue.
  (doom-modeline-project-detection 'auto)

  ;; Determines the style used by `doom-modeline-buffer-file-name'.
  ;;
  ;; Given ~/Projects/FOSS/emacs/lisp/comint.el
  ;;   auto => emacs/l/comint.el (in a project) or comint.el
  ;;   truncate-upto-project => ~/P/F/emacs/lisp/comint.el
  ;;   truncate-from-project => ~/Projects/FOSS/emacs/l/comint.el
  ;;   truncate-with-project => emacs/l/comint.el
  ;;   truncate-except-project => ~/P/F/emacs/l/comint.el
  ;;   truncate-upto-root => ~/P/F/e/lisp/comint.el
  ;;   truncate-all => ~/P/F/e/l/comint.el
  ;;   truncate-nil => ~/Projects/FOSS/emacs/lisp/comint.el
  ;;   relative-from-project => emacs/lisp/comint.el
  ;;   relative-to-project => lisp/comint.el
  ;;   file-name => comint.el
  ;;   file-name-with-project => FOSS|comint.el
  ;;   buffer-name => comint.el<2> (uniquify buffer name)
  ;;
  ;; If you are experiencing the laggy issue, especially while editing remote files
  ;; with tramp, please try `file-name' style.
  ;; Please refer to https://github.com/bbatsov/projectile/issues/657.
  (doom-modeline-buffer-file-name-style 'auto)

  ;; Whether display icons in the mode-line.
  ;; While using the server mode in GUI, should set the value explicitly.
  (doom-modeline-icon t)

  ;; Whether display the icon for `major-mode'. It respects option `doom-modeline-icon'.
  (doom-modeline-major-mode-icon t)

  ;; Whether display the colorful icon for `major-mode'.
  ;; It respects `nerd-icons-color-icons'.
  (doom-modeline-major-mode-color-icon t)

  ;; Whether display the icon for the buffer state. It respects option `doom-modeline-icon'.
  (doom-modeline-buffer-state-icon t)

  ;; Whether display the modification icon for the buffer.
  ;; It respects option `doom-modeline-icon' and option `doom-modeline-buffer-state-icon'.
  (doom-modeline-buffer-modification-icon t)

  ;; Whether display the lsp icon. It respects option `doom-modeline-icon'.
  (doom-modeline-lsp-icon t)

  ;; Whether display the time icon. It respects option `doom-modeline-icon'.
  (doom-modeline-time-icon t)

  ;; Whether display the live icons of time.
  ;; It respects option `doom-modeline-icon' and option `doom-modeline-time-icon'.
  (doom-modeline-time-live-icon t)

  ;; Whether to use an analogue clock svg as the live time icon.
  ;; It respects options `doom-modeline-icon', `doom-modeline-time-icon', and `doom-modeline-time-live-icon'.
  (doom-modeline-time-analogue-clock t)

  ;; The scaling factor used when drawing the analogue clock.
  (doom-modeline-time-clock-size 0.7)

  ;; Whether to use unicode as a fallback (instead of ASCII) when not using icons.
  (doom-modeline-unicode-fallback nil)

  ;; Whether display the buffer name.
  (doom-modeline-buffer-name t)

  ;; Whether highlight the modified buffer name.
  (doom-modeline-highlight-modified-buffer-name t)

  ;; When non-nil, mode line displays column numbers zero-based.
  ;; See `column-number-indicator-zero-based'.
  (doom-modeline-column-zero-based t)

  ;; Specification of \"percentage offset\" of window through buffer.
  ;; See `mode-line-percent-position'.
  (doom-modeline-percent-position '(-3 "%p"))

  ;; Format used to display line numbers in the mode line.
  ;; See `mode-line-position-line-format'.
  (doom-modeline-position-line-format '("L%l"))

  ;; Format used to display column numbers in the mode line.
  ;; See `mode-line-position-column-format'.
  (doom-modeline-position-column-format '("C%c"))

  ;; Format used to display combined line/column numbers in the mode line. See `mode-line-position-column-line-format'.
  (doom-modeline-position-column-line-format '("%l:%c"))

  ;; Whether display the minor modes in the mode-line.
  (doom-modeline-minor-modes nil)

  ;; If non-nil, a word count will be added to the selection-info modeline segment.
  (doom-modeline-enable-word-count nil)

  ;; Major modes in which to display word count continuously.
  ;; Also applies to any derived modes. Respects `doom-modeline-enable-word-count'.
  ;; If it brings the sluggish issue, disable `doom-modeline-enable-word-count' or
  ;; remove the modes from `doom-modeline-continuous-word-count-modes'.
  (doom-modeline-continuous-word-count-modes '(markdown-mode gfm-mode org-mode))

  ;; Whether display the buffer encoding.
  (doom-modeline-buffer-encoding t)

  ;; Whether display the indentation information.
  (doom-modeline-indent-info nil)

  ;; Whether display the total line number。
  (doom-modeline-total-line-number nil)

  ;; Whether display the icon of vcs segment. It respects option `doom-modeline-icon'."
  (doom-modeline-vcs-icon t)

  ;; The maximum displayed length of the branch name of version control.
  (doom-modeline-vcs-max-length 15)

  ;; The function to display the branch name.
  (doom-modeline-vcs-display-function #'doom-modeline-vcs-name)

  ;; Whether display the icon of check segment. It respects option `doom-modeline-icon'.
  (doom-modeline-check-icon t)

  ;; If non-nil, only display one number for check information if applicable.
  (doom-modeline-check-simple-format nil)

  ;; The maximum number displayed for notifications.
  (doom-modeline-number-limit 99)

  ;; Whether display the workspace name. Non-nil to display in the mode-line.
  (doom-modeline-workspace-name t)

  ;; Whether display the perspective name. Non-nil to display in the mode-line.
  (doom-modeline-persp-name t)

  ;; If non nil the default perspective name is displayed in the mode-line.
  (doom-modeline-display-default-persp-name nil)

  ;; If non nil the perspective name is displayed alongside a folder icon.
  (doom-modeline-persp-icon t)

  ;; Whether display the `lsp' state. Non-nil to display in the mode-line.
  (doom-modeline-lsp t)

  ;; Whether display the GitHub notifications. It requires `ghub' package.
  (doom-modeline-github nil)

  ;; The interval of checking GitHub.
  (doom-modeline-github-interval (* 30 60))

  ;; Whether display the modal state.
  ;; Including `evil', `overwrite', `god', `ryo' and `xah-fly-keys', etc.
  (doom-modeline-modal t)

  ;; Whether display the modal state icon.
  ;; Including `evil', `overwrite', `god', `ryo' and `xah-fly-keys', etc.
  (doom-modeline-modal-icon t)

  ;; Whether display the modern icons for modals.
  (doom-modeline-modal-modern-icon t)

  ;; When non-nil, always show the register name when recording an evil macro.
  (doom-modeline-always-show-macro-register nil)

  ;; Whether display the mu4e notifications. It requires `mu4e-alert' package.
  (doom-modeline-mu4e nil)
  ;; also enable the start of mu4e-alert
  (mu4e-alert-enable-mode-line-display)

  ;; Whether display the gnus notifications.
  (doom-modeline-gnus t)

  ;; Whether gnus should automatically be updated and how often (set to 0 or smaller than 0 to disable)
  (doom-modeline-gnus-timer 2)

  ;; Wheter groups should be excludede when gnus automatically being updated.
  (doom-modeline-gnus-excluded-groups '("dummy.group"))

  ;; Whether display the IRC notifications. It requires `circe' or `erc' package.
  (doom-modeline-irc t)

  ;; Function to stylize the irc buffer names.
  (doom-modeline-irc-stylize 'identity)

  ;; Whether display the battery status. It respects `display-battery-mode'.
  (doom-modeline-battery t)

  ;; Whether display the time. It respects `display-time-mode'.
  (doom-modeline-time t)

  ;; Whether display the misc segment on all mode lines.
  ;; If nil, display only if the mode line is active.
  (doom-modeline-display-misc-in-all-mode-lines t)

  ;; The function to handle `buffer-file-name'.
  (doom-modeline-buffer-file-name-function #'identity)

  ;; The function to handle `buffer-file-truename'.
  (doom-modeline-buffer-file-truename-function #'identity)

  ;; Whether display the environment version.
  (doom-modeline-env-version t)
  ;; Or for individual languages
  (doom-modeline-env-enable-python t)
  (doom-modeline-env-enable-ruby t)
  (doom-modeline-env-enable-perl t)
  (doom-modeline-env-enable-go t)
  (doom-modeline-env-enable-elixir t)
  (doom-modeline-env-enable-rust t)

  ;; Change the executables to use for the language version string
  (doom-modeline-env-python-executable "python") ; or `python-shell-interpreter'
  (doom-modeline-env-ruby-executable "ruby")
  (doom-modeline-env-perl-executable "perl")
  (doom-modeline-env-go-executable "go")
  (doom-modeline-env-elixir-executable "iex")
  (doom-modeline-env-rust-executable "rustc")

  ;; What to display as the version while a new one is being loaded
  (doom-modeline-env-load-string "...")

  ;; By default, almost all segments are displayed only in the active window. To
  ;; display such segments in all windows, specify e.g.
  (doom-modeline-always-visible-segments '(mu4e irc))

  ;; Hooks that run before/after the modeline version string is updated
  (doom-modeline-before-update-env-hook nil)
  (doom-modeline-after-update-env-hook nil))

;; ;; loading major ui changes
(erasmo-ui--init-theme 'doom-zenburn)
(doom-modeline-mode 1)
(display-time-mode 1)

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
