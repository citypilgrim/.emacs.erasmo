;; -*- lexical-binding: t; -*-
(require 'flycheck)

;; columne numbering in modeline
(column-number-mode)

;; Enable line numbers for some modes
(dolist (mode '(text-mode-hook
                prog-mode-hook
                conf-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 1))))

;; Override some modes which derive from the above
(dolist (mode '(org-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; text wrapping
(add-hook 'prog-mode-hook '(lambda () (toggle-truncate-lines 1)))

;; linting
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(provide 'erasmo-ide)
