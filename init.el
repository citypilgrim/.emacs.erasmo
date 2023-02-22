;; -*- lexical-binding: t; -*-

;; Profile emacs startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s seconds with %d garbage collections."
                     (emacs-init-time "%.2f")
                     gcs-done)))

;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

(add-hook 'after-init-hook (lambda ()
                             ;; Make gc pauses faster by decreasing the threshold.
                             (setq gc-cons-threshold (* 2 1000 1000))))

;; Add configuration modules to load path
(add-to-list 'load-path
	     (concat user-emacs-directory "modules"))

;; Load pertinent modules
;; order matters
(require 'erasmo-package)
(require 'erasmo-performance)
(require 'erasmo-defaults)
;; order agnostic
(require 'erasmo-keybind)
(require 'erasmo-ux)
(require 'erasmo-ui)
(require 'erasmo-ide)
(require 'erasmo-lisp)
(require 'erasmo-vc)

