;; -*- lexical-binding: t; -*-

;; setting shortcut for 'M-x replace-regex/string'
(global-set-key (kbd "M-s r") 'replace-regexp)
(global-set-key (kbd "M-s s") 'replace-string)

;; setting shortcut for C-x C-s
(global-set-key (kbd "C-S-a") (lambda ()
                                (interactive)
                                (save-buffer)))
(global-set-key (kbd "C-S-c") (lambda ()
                                (interactive)
                                (whitespace-cleanup)
                                (save-buffer)))

;; Switching windows backwards
(global-set-key (kbd "C-x O") (lambda ()
                                (interactive)
                                (other-window -1)))

(provide 'erasmo-keybind)
