;; -*- lexical-binding: t; -*-

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

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

;; timer
(use-package tmr)

(defun erasmo-ui--tmr-mode-line ()
  (if (not (and (boundp 'tmr--timers)
                tmr--timers))
      ""
    (propertize (format "timer %s: %s"
                        (tmr--format-remaining (car tmr--timers))
                        (tmr--timer-description (car tmr--timers)))
                'tab-bar '(:foreground "orange"))))

;; tab bar workspaces
(defun erasmo-ux--set-tab-bar-faces ()
  (let ((color (face-attribute 'doom-modeline-bar :background nil t)))
    (set-face-attribute 'tab-bar-tab nil :foreground nil :background nil :weight 'semi-bold :underline `(:color ,color) :inherit nil)
    (set-face-attribute 'tab-bar nil :font "Iosevka Aile" :foreground nil :inherit 'mode-line)))

;; (setq tab-bar-close-button-show nil
;;       tab-bar-format '(tab-bar-format-history
;;                        tab-bar-format-tabs-groups
;;                        tab-bar-separator
;;                        erasmo-ui--tmr-mode-line
;;                        tab-bar-separator
;;                        tab-bar-format-align-right
;;                        tab-bar-format-global))

(setq tab-line-format
      '((:eval (format-time-string "%H:%M"))
        (tab-bar-format-align-right))
      tab-line-close-button-show nil)

(with-eval-after-load 'doom-modeline
  (erasmo-ux--set-tab-bar-faces)

  ;; (add-to-list 'global-mode-string '(" " display-time-string))
  ;; (add-to-list 'global-mode-string '(" " doom-modeline--battery-status))
  ;; (add-to-list 'global-mode-string '(" " tracking-mode-line-buffers))

  ;; (display-time-mode 1)
  ;; (display-battery-mode 1)

  (setq tab-bar-show t)
  (tab-bar-mode 1)
  (tab-bar-rename-tab (getenv "USER")))

(use-package tabspaces
  :commands (tabspaces-switch-or-create-workspace
             tabspaces-open-or-create-project-and-workspace)
  :custom
  (tabspaces-use-filtered-buffers-as-default t)
  (tabspaces-default-tab "Default")
  (tabspaces-remove-to-default t)
  (tabspaces-include-buffers '("*scratch*"))
  ;; sessions
  ;; (tabspaces-session t)
  ;; (tabspaces-session-auto-restore t)
  )
(tabspaces-mode)
  

;; TODO configure consult to be local to the tab bar
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


;; nice documentation
(use-package helpful
  :bind
  ("C-h p" . helpful-at-point))

;; project management
(use-package project
  :bind ("C-x p g" . consult-ripgrep))

(provide 'erasmo-ux)
