;; -*- lexical-binding: t; -*-

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

;; Window configuration for special windows.
;; This section inspired by the article "Demystifying Emacs’s Window
;; Manager" found here:
;; https://www.masteringemacs.org/article/demystifying-emacs-window-manager
(add-to-list 'display-buffer-alist
             '("\\*Help\\*"
               (display-buffer-reuse-window display-buffer-pop-up-window)))

(add-to-list 'display-buffer-alist
             '("\\*Completions\\*"
               (display-buffer-reuse-window display-buffer-pop-up-window)
               (inhibit-same-window . t)
               (window-height . 10)))

;; Show dictionary definition on the left
(add-to-list 'display-buffer-alist
             '("^\\*Dictionary\\*"
               (display-buffer-in-side-window)
               (side . left)
               (window-width . 70)))
;; define a key to define the word at point.
(define-key global-map (kbd "M-#") #'dictionary-lookup-definition)

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


;; tab bar workspaces
(use-package tabspaces
  :commands (tabspaces-switch-or-create-workspace
             tabspaces-open-or-create-project-and-workspace)
  :custom
  (tabspaces-use-filtered-buffers-as-default nil)
  (tabspaces-default-tab "Default")
  (tabspaces-remove-to-default t)
  ;; (tabspaces-include-buffers '("*scratch*" "*Messages*")) ;this doesnt help
  ;; sessions
  (tabspaces-session t)
  (tabspaces-session-auto-restore nil)
  )
(tabspaces-mode)

(with-eval-after-load 'consult
  ;; Hide full buffer list by default (still available with "b" prefix)
  (consult-customize consult--source-buffer :hidden t :default nil)

  ;; Set consult-workspace buffer list
  (defvar consult--source-workspace
    (list :name "Workspace Buffers"
          :narrow ?w
          :history 'buffer-name-history
          :category 'buffer
          :state #'consult--buffer-state
          :default t
          :items (lambda () (consult--buffer-query
                             :predicate #'tabspaces--local-buffer-p
                             :sort 'visibility
                             :as #'buffer-name)))

    "Set workspace buffer list for consult-buffer.")
  (add-to-list 'consult-buffer-sources 'consult--source-workspace))

(defun erasmo-ux-switch-tab-buffer (&optional arg)
  (interactive "P")
  (cond
   ((and arg (> (car arg) 0)) (call-interactively #'consult-buffer))
   ((project-current) (call-interactively #'project-switch-to-buffer))
   (t (call-interactively #'consult-buffer))))

(global-set-key (kbd "C-M-p") #'tab-bar-switch-to-prev-tab)
(global-set-key (kbd "C-M-n") #'tab-bar-switch-to-next-tab)

;; tweaking dired
(use-package emacs
  :bind (:map dired-mode-map
              ("M-<return>" . #'dired-up-directory))
  :config
  (setq dired-listing-switches "-ahl --group-directories-first"
        dired-omit-verbose nil)

  :init
  (add-hook 'dired-mode-hook
            (lambda ()
              (interactive)
              (dired-omit-mode 0)
              (hl-line-mode 1)
              (dired-async-mode)
              (diminish 'dired-async-mode)
              (toggle-truncate-lines 1))))

;; mini pop up buffers with popper
(use-package popper
  :bind (("C-M-`" . popper-toggle-latest)
         ("M-`" . popper-cycle)           ;close popup
         ("C-M-\"" . popper-toggle-type)) ;change popup to proper buffer
  :init
  (popper-mode)
  :custom
  (popper-window-height 12)
  (popper-reference-buffers '(eshell-mode
                              vterm-mode
                              geiser-repl-mode
                              help-mode
                              grep-mode
                              helpful-mode
                              compilation-mode)))

;; control bugger placement
(setq display-buffer-base-action
      '(display-buffer-reuse-mode-window
        display-buffer-reuse-window
        display-buffer-same-window))

;; If a popup does happen, don't resize windows to be equal-sized
(setq even-window-sizes nil)


;;  documentation
(use-package helpful
  :bind
  ("C-h p" . helpful-at-point))

(use-package elisp-demos)               ;also add some examples
(advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update)

;; project management
(use-package project
  :bind ("C-x p g" . consult-ripgrep))

(provide 'erasmo-ux)
