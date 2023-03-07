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

(provide 'erasmo-package)
