;; -*- lexical-binding: t; -*-

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
   "!" '(:ignore t :which-key "flycheck")
   "&" '(:ignore t :which-key "yasnippet")
   "b" '(:ignore t :which-key "biblio")
   "c" '(:ignore t :which-key "calendar")
   "d" '(:ignore t :which-key "debug")
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

(provide 'erasmo-keybind)
