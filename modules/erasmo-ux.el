;; -*- lexical-binding: t; -*-

(use-package undo-fu
  :config
  (global-set-key (kbd "C-/") 'undo-fu-only-undo)
  (global-set-key (kbd "C-.") 'undo-fu-only-redo))

;; replace-string command to be case sensitive
(defadvice replace-string (around turn-off-case-fold-search)
  (let ((case-fold-search nil))
    ad-do-it))
(ad-activate 'replace-string)

(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)) ;one line at a time
      mouse-wheel-progressive-speed nil            ;dont accelerate scrolling
      mouse-wheel-follow-mouse 't                  ;scroll window under mouse
      scroll-step 1                                ;keyboard scroll one line
      )

(use-package helpful
  :bind
  ("C-h p" . helpful-at-point))

(provide 'erasmo-ux)
