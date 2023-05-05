;; -*- lexical-binding: t; -*-
(require 'markdown-mode)

(with-eval-after-load 'markdown-mode
  (setq markdown-header-scaling t)
  (setq markdown-header-scaling-values
        '(1.2 1.1 1.1 1.0 1.0 0.9))
  (setq markdown-hide-urls t)
  (setq markdown-hide-markup t)
  ;; TODO: Package js/css for prettier previews
  (setq markdown-command "pandoc")
  (setq markdown-fontify-code-blocks-natively t))

(provide 'erasmo-markup)
