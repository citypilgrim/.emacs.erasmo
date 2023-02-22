;; -*- lexical-binding: t; -*-

(use-package magit
  :bind ("C-M-:" . magit-status)
  :commands (magit-status magit-get-current-branch)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(require 'erasmo-vc)
