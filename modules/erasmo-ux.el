;; -*- lexical-binding: t; -*-

;; prevent emacs from popping new windows when we run commands
(setq display-buffer-base-action
      '(display-buffer-reuse-mode-window
        display-buffer-reuse-window
        display-buffer-same-window))

;; If a popup does happen, don't resize windows to be equal-sized
(setq even-window-sizes nil)

;; enabling redo
(use-package undo-fu
  :config
  (global-set-key (kbd "C-/") 'undo-fu-only-undo)
  (global-set-key (kbd "C-.") 'undo-fu-only-redo))

;; replace-string command to be case sensitive
(defadvice replace-string (around turn-off-case-fold-search)
  (let ((case-fold-search nil))
    ad-do-it))
(ad-activate 'replace-string)

;; mouse scrolling
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)) ;one line at a time
      mouse-wheel-progressive-speed nil            ;dont accelerate scrolling
      mouse-wheel-follow-mouse 't                  ;scroll window under mouse
      scroll-step 1                                ;keyboard scroll one line
      )

;; nice documentation
(use-package helpful
  :bind
  ("C-h p" . helpful-at-point))

(provide 'erasmo-ux)
