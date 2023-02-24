;; -*- lexical-binding: t; -*-

;; minimalist look
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

;; time on modeline
(setenv "TZ" "UTC-8")
(display-time-mode 1)
(customize-set-variable 'display-time-format "%l:%M %p %b %y")
(customize-set-variable 'display-time-default-load-average nil)

;; fonts
(defvar erasmo-ui/font-scale-factor 0.85)
(defvar erasmo-ui/font-size 142)

;; fix up the font setting function
(defun erasmo-ui/set-font-size (&optional SCALE-FACTOR)
  (interactive "nFont scale factor: ")
  (if SCALE-FACTOR
      (setq erasmo-ui/font-scale-factor SCALE-FACTOR))
  (erasmo-ui/set-font))

(defun erasmo-ui/set-font ()
  ;; default font face
  (set-face-attribute 'default nil
                      :height (ceiling (* erasmo-ui/font-size erasmo-ui/font-scale-factor)))

  ;; Set the fixed pitch face
  (set-face-attribute 'fixed-pitch nil
                      :height (ceiling (* erasmo-ui/font-size erasmo-ui/font-scale-factor)))

  ;; Set the variable pitch face
  (set-face-attribute 'variable-pitch nil
                      :height (ceiling (* erasmo-ui/font-size erasmo-ui/font-scale-factor))))

(erasmo-ui/set-font)

(use-package all-the-icons
  :custom
  (all-the-icons-scale-factor 1))

;; for completion buffers
(use-package all-the-icons-completion
  :init
  (all-the-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'all-the-icons-completion-marginalia-setup))

;; mode line
(use-package smart-mode-line
  :disabled
  :config
  (setq sml/no-confirm-load-theme t)
  (sml/setup)
  (sml/apply-theme 'respectful)  ; Respect the theme colors
  (setq sml/mode-width 'right
        sml/name-width 60)

  (setq rm-excluded-modes
	(mapconcat
         'identity
					; These names must start with a space!
         '(" GitGutter" " MRev" " company"
           " Helm" " Undo-Tree" " Projectile.*" " Z" " Ind"
           " Org-Agenda.*" " ElDoc" " SP/s" " cider.*")
         "\\|")))


;; Themes
;; A nice gallery of Emacs themes can be found at https://emacsthemes.com/.

(use-package spacegray-theme :defer t)

(use-package doom-themes
  :defer t
  :custom
  (doom-themes-visual-bell-config)
  )

(use-package darktooth-theme :defer t)

(use-package ample-theme
  :init (progn (load-theme 'ample t t)
               (load-theme 'ample-flat t t)
               (load-theme 'ample-light t t))
  :defer t
  :ensure t)

(use-package autumn-light-theme :defer t)

(defun erasmo-ui/load-theme (theme)
  "Enhance `load-theme' by first disabling enabled themes."
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme theme t)
  ;; (if (string-equal major-mode "org-mode")
  ;;     (progn
  ;;       (peregrino/org-mode-setup)
  ;;       (peregrino/set-org-agenda-files)))
  )

(erasmo-ui/load-theme 'doom-zenburn)

(provide 'erasmo-ui)
