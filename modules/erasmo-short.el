;; -*- lexical-binding: t; -*-
;; keybindings put together by me, that are not strongly related
;; to any package
(require 'erasmo-keybind)


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
;; buffer viewing
(erasmo-keybind-leader-key-def
 "tt" 'toggle-truncate-lines
 "tv" 'visual-fill-column-mode)

(defun erasmo-short--search-files (directory)
  (interactive)
  (consult-ripgrep (expand-file-name directory) ""))

;; file browsing
(erasmo-keybind-leader-key-def
  ;; quick access
  "fb" '((lambda () (interactive) (find-file "~/handbook/biblio.bib")) :which-key "biblio")
  "fc" '((lambda () (interactive) (consult-find "~/.emacs.d" "erasmo-")) :which-key "config")
  "fC" '((lambda () (interactive) (find-file "~/.emacs.d/init.el")) :which-key "config")
  "fg" '((lambda () (interactive) (find-file "~/.dotfiles/erasmo/systems/base.scm")) :which-key "guix")
  "fs" '((lambda () (interactive) (find-file "~/handbook/slipbox.org")) :which-key "slipbox")

  ;; notes
  "na" '((lambda () (interactive) (counsel-file-jump "" "~/handbook/agenda")) :which-key "agenda")
  "no" '((lambda () (interactive) (counsel-file-jump "" "~/handbook/org")) :which-key "org")
  "ns" '(:ignore t :which-key "search files")
  "nsa" '((lambda () (interactive) (erasmo-short--search-files "~/handbook/agenda")) :which-key "agenda")
  "nso" '((lambda () (interactive) (erasmo-short--search-files "~/handbook/org")) :which-key "org")
  "nsf" '((lambda () (interactive) (erasmo-short--search-files "~/handbook/roam")) :which-key "roam")
  "nsd" '((lambda () (interactive) (erasmo-short--search-files "~/handbook/journal")) :which-key "dailies"))

(provide 'erasmo-short)
