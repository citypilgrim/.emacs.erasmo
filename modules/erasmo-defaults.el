;; -*- lexical-binding: t; -*-

;; keeping emacs clean
(use-package no-littering)

;; removing safety locks
(put 'narrow-to-region 'disabled nil)

;; Keep customization settings in a temporary file
(setq custom-file
      (if (boundp 'server-socket-dir)
          (expand-file-name "custom.el" server-socket-dir)
        (expand-file-name (format "emacs-custom-%s.el" (user-uid)) temporary-file-directory)))
(load custom-file t)

;; override marked regions
(delete-selection-mode 1)

;; Revert Dired and other buffers
(customize-set-variable 'global-auto-revert-non-file-buffers t)

;; Revert buffers when the underlying file has changed
(global-auto-revert-mode 1)

;; Use spaces instead of tabs
(setq-default indent-tabs-mode nil)

;; Use "y" and "n" to confirm/negate prompt instead of "yes" and "no"
;; Using `advice' here to make it easy to reverse in custom
;; configurations with `(advice-remove 'yes-or-no-p #'y-or-n-p)'
;;
;; N.B. Emacs 28 has a variable for using short answers, which should
;; be preferred if using that version or higher.
(if (boundp 'use-short-answers)
    (setq use-short-answers t)
  (advice-add 'yes-or-no-p :override #'y-or-n-p))

(add-hook 'after-init-hook #'recentf-mode)

;; Do not save duplicates in kill-ring
(customize-set-variable 'kill-do-not-save-duplicates t)

;; Better support for files with long lines
(setq-default bidi-paragraph-direction 'left-to-right)
(setq-default bidi-inhibit-bpa t)
(global-so-long-mode 1)

;; entering gpg password via emacs
(use-package pinentry
  :init (setq epg-pinentry-mode 'loopback)
  :config (pinentry-start))

;; shorten package names
;; According to [[https://www.emacswiki.org/emacs/DiminishedModes][Emacs Wiki]], the ~:diminish~ flag in ~use-package~ does not work on mode lines that use ~:eval~ forms. So we have to insert a small hack after each of these packages. Below is an example code, but also needed for ~buffer-face-mode~
(use-package diminish
  :init
  (eval-after-load "face-remap" '(diminish 'buffer-face-mode)))t

(provide 'erasmo-defaults)
