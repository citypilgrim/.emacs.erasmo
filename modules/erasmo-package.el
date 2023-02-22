;; -*- lexical-binding: t; -*-

;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
       ("melpa-stable" . "https://stable.melpa.org/packages/")
       ("org" . "https://orgmode.org/elpa/")
       ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)

;; Refresh package contents if it's the first time running this config
(unless package-archive-contents (package-refresh-contents))

;; use-package for package declaration
(require 'use-package)
(setq use-package-always-ensure t)

;; shorten package names
;; According to [[https://www.emacswiki.org/emacs/DiminishedModes][Emacs Wiki]], the ~:diminish~ flag in ~use-package~ does not work on mode lines that use ~:eval~ forms. So we have to insert a small hack after each of these packages. Below is an example code, but also needed for ~buffer-face-mode~
(use-package diminish
  :init
  (eval-after-load "face-remap" '(diminish 'buffer-face-mode)))

(provide 'erasmo-package)
