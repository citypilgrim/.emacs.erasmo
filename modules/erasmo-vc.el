;; -*- lexical-binding: t; -*-

(use-package magit
  :bind ("C-M-:" . magit-status)
  :commands (magit-status magit-get-current-branch)
  :custom
  (magit-display-buffer-function
   #'magit-display-buffer-same-window-except-diff-v1)
  :init
  (erasmo-keybind-leader-key-def
    "gs" 'magit-status
    "gd" 'magit-diff-unstaged
    "gc" 'magit-branch-or-checkout
    "gl" '(:ignore t :which-key "log")
    "glc" 'magit-log-current
    "glf" 'magit-log-buffer-file
    "gb" 'magit-branch
    "gP" 'magit-push-current
    "gp" 'magit-pull-branch
    "gf" 'magit-fetch
    "gF" 'magit-fetch-all
    "gr" 'magit-rebase)
  (add-to-list 'transient-levels '((magit-commit
                                    (magit:--gpg-sign . 1)))))

;; code review with forge
;; Most of the forge commands can be retrieved from it's transient buffer. To edit currently visited issue with easy through key bindings, we utilise the the following key maps:
;; 1. ~C-c C-e~ on the field to edit
;; 2. ~C-c C-n~ to add a new comment
(use-package forge
  :after magit
  :config
  (if erasmo-env-forge-alist
      (add-to-list 'forge-alist erasmo-env-forge-alist)))

;; show todos in git status
(use-package magit-todos
  :hook (magit-status . magit-todos-mode)
  :config
  (setq magit-todos-recursive t
	magit-todos-depth 100)
  (setq magit-todos-ignore-file-suffixes '("todo")))

;; tracking changes with git-gutter
(use-package git-gutter
  ;; :disabled t
  :diminish git-gutter-mode
  :hook ((text-mode . git-gutter-mode)
	 (prog-mode . git-gutter-mode))
  :config
  (setq git-gutter:update-interval 2)
  (use-package git-gutter-fringe)
  (use-package git-gutter-fringe)
  (set-face-foreground 'git-gutter-fr:added "LightGreen")
  (fringe-helper-define 'git-gutter-fr:added nil
			"XXXXXXXXXX"
			"XXXXXXXXXX"
			"XXXXXXXXXX"
			".........."
			".........."
			"XXXXXXXXXX"
			"XXXXXXXXXX"
			"XXXXXXXXXX"
			".........."
			".........."
			"XXXXXXXXXX"
			"XXXXXXXXXX"
			"XXXXXXXXXX")

  (set-face-foreground 'git-gutter-fr:modified "LightGoldenrod")
  (fringe-helper-define 'git-gutter-fr:modified nil
			"XXXXXXXXXX"
			"XXXXXXXXXX"
			"XXXXXXXXXX"
			".........."
			".........."
			"XXXXXXXXXX"
			"XXXXXXXXXX"
			"XXXXXXXXXX"
			".........."
			".........."
			"XXXXXXXXXX"
			"XXXXXXXXXX"
			"XXXXXXXXXX")

  (set-face-foreground 'git-gutter-fr:deleted "LightCoral")
  (fringe-helper-define 'git-gutter-fr:deleted nil
			"XXXXXXXXXX"
			"XXXXXXXXXX"
			"XXXXXXXXXX"
			".........."
			".........."
			"XXXXXXXXXX"
			"XXXXXXXXXX"
			"XXXXXXXXXX"
			".........."
			".........."
			"XXXXXXXXXX"
			"XXXXXXXXXX"
			"XXXXXXXXXX")

  ;; These characters are used in terminal mode
  (setq git-gutter:modified-sign "≡")
  (setq git-gutter:added-sign "≡")
  (setq git-gutter:deleted-sign "≡")
  (set-face-foreground 'git-gutter:added "LightGreen")
  (set-face-foreground 'git-gutter:modified "LightGoldenrod")
  (set-face-foreground 'git-gutter:deleted "LightCoral"))


;; package management with guix
(use-package guix)

(provide 'erasmo-vc)
