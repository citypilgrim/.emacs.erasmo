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

;; straight.el to install those packages not published on melpa
;; treat this as a last resort
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(provide 'erasmo-package)
