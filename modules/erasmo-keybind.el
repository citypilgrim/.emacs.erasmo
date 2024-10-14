;; -*- lexical-binding: t; -*-

(global-set-key (kbd "C-x b") #'ibuffer)

;; which-key; transient preview of keybindings
(defun erasmo-keybind--diminish-which-key ()
  (eval-after-load "which-key" '(diminish 'which-key-mode)))

(use-package which-key
  :init (which-key-mode)
  :config
  (setq which-key-idle-delay 0.3)
  (erasmo-keybind--diminish-which-key)
  :diminish which-key-mode
  )

;; general; prefixed keybindings
(use-package general
  :config
  (general-create-definer erasmo-keybind-leader-key-def
    :prefix "C-c")
  (general-create-definer erasmo-keybind-alt-leader-key-def
    :prefix "C-x")
  (erasmo-keybind-leader-key-def
    "." '(:ignore t :which-key "smudge")
    "!" '(:ignore t :which-key "flycheck")
    "&" '(:ignore t :which-key "yasnippet")
    "b" '(:ignore t :which-key "biblio")
    "c" '(:ignore t :which-key "calendar")
    "d" '(:ignore t :which-key "docker")
    "e" '(:ignore t :which-key "eval")
    "f" '(:ignore t :which-key "files")
    "g" '(:ignore t :which-key "git")
    "j" '(:ignore t :which-key "jump")
    "l" '(:ignore t :which-key "lsp")
    "n" '(:ignore t :which-key "notes")
    "o" '(:ignore t :which-key "org")
    "p" '(:ignore t :which-key "projectile")
    "s" '(:ignore t :which-key "sword")
    "t" '(:ignore t :which-key "toggles"))
  (erasmo-keybind-alt-leader-key-def
    "a" '(:ignore t :which-key "abbrev")
    "n" '(:ignore t :which-key "narrow")
    "p" '(:ignore t :which-key "project")
    "r" '(:ignore t :which-key "rectangle & register")
    "t" '(:ignore t :which-key "tab")
    "x" '(:ignore t :which-key "buffer")
    "X" '(:ignore t :which-key "edebug")
    "RET" '(:ignore t :which-key "code sys")
    "8" '(:ignore t :which-key "insert char")))

;; hydra; stateful key maps
(use-package hydra
  :defer 1)

;; jumping within buffer with avy
(use-package avy
  :commands (avy-goto-char avy-goto-word-0 avy-goto-line)
  :init
  (erasmo-keybind-leader-key-def
   "jj" '(avy-goto-char :which-key "jump to char")
   "jw" '(avy-goto-word-0 :which-key "jump to word")
   "jl" '(avy-goto-line :which-key "jump to line")))

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

(defun erasmo-keybind--search-files (directory)
  (interactive)
  (consult-ripgrep (expand-file-name directory) ""))

;; file browsing
(erasmo-keybind-leader-key-def
  ;; quick access
  "fb" '((lambda () (interactive) (find-file erasmo-env-biblio)) :which-key "biblio")
  "fc" '((lambda () (interactive) (consult-find "~/.emacs.d" "erasmo-")) :which-key "config")
  "fC" '((lambda () (interactive) (find-file "~/.emacs.d/init.el")) :which-key "config")
  "fe" '((lambda () (interactive) (find-file "~/.env/emacs/erasmo-env.el")) :which-key "env")
  "fg" '((lambda () (interactive) (find-file "~/src/erasmo/examples/src/citypilgrim/packages.scm")) :which-key "guix")
  "fs" '((lambda () (interactive) (find-file erasmo-env-slipbox)) :which-key "slipbox")

  "fl" '(:ignore t :which-key "library")
  "flc" '((lambda () (interactive) (consult-find "~/library/christian")) :which-key "christian")
  "fls" '((lambda () (interactive) (consult-find "~/library/software")) :which-key "software")

  ;; notes
  "na" '((lambda () (interactive) (consult-find erasmo-env-agenda-directory)) :which-key "agenda")
  "nA" '((lambda () (interactive) (consult-find erasmo-env-default-agenda-directory)) :which-key "default agenda")
  "no" '((lambda () (interactive) (consult-find erasmo-env-org-directory)) :which-key "org")
  "ns" '(:ignore t :which-key "search files")
  "nsa" '((lambda () (interactive) (erasmo-keybind--search-files erasmo-env-agenda-directory)) :which-key "agenda")
  "nsA" '((lambda () (interactive) (erasmo-keybind--search-files erasmo-env-default-agenda-directory)) :which-key "default agenda")
  "nso" '((lambda () (interactive) (erasmo-keybind--search-files erasmo-env-org-directory)) :which-key "org")
  "nsf" '((lambda () (interactive) (erasmo-keybind--search-files erasmo-env-org-roam-directory)) :which-key "roam")
  "nsd" '((lambda () (interactive) (erasmo-keybind--search-files erasmo-env-org-roam-dailies-directory)) :which-key "dailies"))

(provide 'erasmo-keybind)
