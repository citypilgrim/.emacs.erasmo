;; -*- lexical-binding: t; -*-

;; core config
(use-package org
  :defer nil
  :hook (org-mode . erasmo-org--mode-setup)
  :custom
  (org-return-follows-link t)
  (org-mouse-1-follows-link t)
  (org-link-descriptive t)
  (org-ellipsis " ▾")
  (org-hide-emphasis-markers t)
  (org-src-fontify-natively t)
  (org-fontify-quote-and-verse-blocks t)
  (org-src-tab-acts-natively t)
  (org-edit-src-content-indentation 2)
  (org-hide-block-startup nil)
  (org-src-preserve-indentation nil)
  (org-startup-folded 'showeverything)
  (org-cycle-separator-lines 2)
  (org-outline-path-complete-in-steps nil) ;refile in a single go
  (org-refile-use-outline-path t)       ;show full path when refiling
  (org-refile-allow-creating-parent-nodes 'confirm) ; allow refile to create parent tasks with confirmation
  :init
  ;; auto tangle on save
  (add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook
                                                #'erasmo-org--babel-tangle-dont-ask
                                                'run-at-end 'only-in-org-mode)))
  ;; org-agenda
  (add-hook 'org-agenda-after-show-hook #'org-mode)
  :config
  (add-to-list 'org-babel-load-languages '(C . t)) ;org blocks should be for "C" not "c"
  (add-to-list 'org-babel-load-languages '(shell . t)))

(defun erasmo-org--babel-tangle-dont-ask ()
  ;; Dynamic scoping to the rescue
  (let ((org-confirm-babel-evaluate nil))
    (org-babel-tangle)))

(defun erasmo-org--mode-setup ()
  ;; turn on indentation and auto-fill mode for org files
  (org-indent-mode)
  (diminish 'org-indent-mode)
  (variable-pitch-mode 1)
  (auto-fill-mode 0)
  (visual-line-mode 1)
  (diminish 'visual-line-mode)
  (flyspell-mode 1)
  (diminish 'flyspell-mode)
  (erasmo-org--setup-org-faces)
  )

(defun erasmo-org--setup-org-faces ()
  ;; Set faces for heading levels
  (set-face-attribute 'org-document-title nil :font "Iosevka Aile"
                      :weight 'bold :height 1.3)
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "Iosevka Aile"
                        :weight 'regular :height (cdr face)))

  ;; fonts
  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-table nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-formula nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)

  ;; Get rid of the background on column views
  (set-face-attribute 'org-column nil :background nil)
  (set-face-attribute 'org-column-title nil :background nil)

  ;; TODO: Others to consider
  ;; '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
  ;; '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
  ;; '(org-property-value ((t (:inherit fixed-pitch))) t)
  ;; '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
  ;; '(org-table ((t (:inherit fixed-pitch :foreground "#83a598"))))
  ;; '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
  ;; '(org-verbatim ((t (:inherit (shadow fixed-pitch))))))
  )

;; modern display
(use-package org-modern)
(global-org-modern-mode)

;; toggle hidden elements
(use-package org-appear
  :hook (org-mode . org-appear-mode))

;; narrowing the view
(use-package visual-fill-column
  :hook (org-mode . erasmo-org--mode-visual-fill))

(defun erasmo-org--mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

;; src blocks short key, this is needed as of Org 9.2
(require 'org-tempo)
(setq geiser-default-implementation 'guile) ;prevent prompt for scheme mode
(add-to-list 'org-structure-template-alist '("sh" . "src sh"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("sc" . "src scheme"))
(add-to-list 'org-structure-template-alist '("ts" . "src typescript"))
(add-to-list 'org-structure-template-alist '("js" . "src javascript"))
(add-to-list 'org-structure-template-alist '("py" . "src python"))
(add-to-list 'org-structure-template-alist '("java" . "src java"))
(add-to-list 'org-structure-template-alist '("yaml" . "src yaml"))
(add-to-list 'org-structure-template-alist '("json" . "src json"))

;; workflow----------

;; task states
;; note that we can filter the task in a buffer by org-sparse-tree
(setq org-todo-keywords
      '((sequence "TODO(t)" "DOING(d)" "|" "DONE(D)")
        (sequence "URGENT" "WAIT(w)" "BACK(b)" "|" "INACTIVE")))

(setq org-todo-keyword-faces
      '(("DOING" . (:foreground "orange red" :weight bold))
        ("URGENT" . (:foreground "red2" :weight bold))
        ("WAIT" . (:foreground "HotPink2" :weight bold))
        ("BACK" . (:foreground "MediumPurple3" :weight bold))
        ("INACTIVE" . (:foreground "purple2" :weight bold))))

(add-hook 'org-after-todo-state-change-hook
          (lambda () (when (equal "DONE" org-state)
                       (call-interactively #'org-archive-to-archive-sibling))))

;; agenda
;; show a timeline of scheduled tasks with org-agenda-list
(defun erasmo-org-set-org-agenda-files ()
  (setq org-agenda-files `(,erasmo-env-agenda-directory ,(concat erasmo-env-agenda-directory "secrets" ) ,erasmo-env-slipbox)))

(add-hook 'after-init-hook #'erasmo-org-set-org-agenda-files)

;; refiling
(setq org-refile-targets '((nil :maxlevel . 1)
                           (org-agenda-files :maxlevel . 1)))

;; seek out other tasks with org-agenda
;; org-agenda-custom-command-template determins the agenda view
(defvar erasmo-org--agenda-custom-command-template
  '((KEY DESCRIPTION
         ((todo "DOING"
                ((org-agenda-overriding-header "Currently Working On")
                 (org-agenda-files AGENDAFILES)))
          (todo "TODO"
                ((agenda "")
                 (org-agenda-files AGENDAFILES)
                 (org-agenda-overriding-header "To do")))
          (todo "URGENT"
                ((org-agenda-overriding-header "Urgent")
                 (org-agenda-files AGENDAFILES)))
          (todo "WAIT"
                ((org-agenda-overriding-header "Waiting For Reply")
                 (org-agenda-files AGENDAFILES)))
          (todo "BACK"
                ((org-agenda-overriding-header "Backlog")
                 (org-agenda-files AGENDAFILES)))
          (todo "INACTIVE"
                ((org-agenda-overriding-header "In Review")
                 (org-agenda-files AGENDAFILES)))))))

(defun erasmo-org--agenda-set-custom-command (TEMPLATE KEY DESCRIPTION AGENDAFILES)
  (setq ttemplate (copy-tree TEMPLATE))
  (setf
   (caar ttemplate) KEY
   (cadar ttemplate) DESCRIPTION
   (cadadr (caddar (caddar ttemplate))) AGENDAFILES
   (cadadr (caddar (cdr (caddar ttemplate)))) AGENDAFILES
   (cadadr (caddar (cddr (caddar ttemplate)))) AGENDAFILES
   (cadadr (caddar (cdddr (caddar ttemplate)))) AGENDAFILES
   (cadadr (caddar (cddddr (caddar ttemplate)))) AGENDAFILES
   (cadadr (caddar (nthcdr 5 (caddar ttemplate)))) AGENDAFILES)
  (identity ttemplate))

(setq org-agenda-custom-commands
      (append
       (erasmo-org--agenda-set-custom-command
        erasmo-org--agenda-custom-command-template
        "p" "personal"
        `(list ,(concat erasmo-env-agenda-directory "/personal-agenda.org")
               ,(concat erasmo-env-agenda-directory "/emacs-agenda.org")
               ))
       ))

(if erasmo-env-org-agenda-custom-commands
    (progn
      (setq erasmo-env-org-agenda-custom-commands
            (mapcar (lambda (e) (car (eval e)))
                    erasmo-env-org-agenda-custom-commands))
      (setq org-agenda-custom-commands
            (append org-agenda-custom-commands
                    erasmo-env-org-agenda-custom-commands))))

;; capture
(setq org-capture-templates
      `(
        ("s" "Slipbox" entry (file ,erasmo-env-slipbox) "* %?\n")))

;; gcal
;; (setq erasmo-org--org-gcal-secret-json
;;       (json-read-file
;;        "~/.emacs-profiles/crafted-emacs/.env/gcal_client_secret.json"))

;; (use-package org-gcal
;;   :init
;;   (setq org-gcal-client-id
;;         (cdr (assoc 'client_id (cdar erasmo-org--org-gcal-secret-json))))
;;   (setq org-gcal-client-secret
;;         (cdr (assoc 'client_secret (cdar erasmo-org--org-gcal-secret-json))))
;;   (setq org-gcal-recurring-events-mode 'top-level)
;;   (setq org-gcal-remove-api-cancelled-events t) ;; No prompt when del removed events
;;   (setq org-gcal-file-alist `((,erasmo-env-email . ,erasmo-env-org-gcal-file)))
;;   )

;; presentation----------
(use-package hide-mode-line)

(defun erasmo-org--presentation-setup ()
  ;; Hide the mode line
  (hide-mode-line-mode 1)

  ;; Display images inline
  (org-display-inline-images) ;; Can also use org-startup-with-inline-images

  ;; This option is more advanced, allows you to scale other faces too
  ;; (setq-local face-remapping-alist '((default (:height 2.0) variable-pitch)
  ;;                                    (org-verbatim (:height 1.75) org-verbatim)
  ;;                                    (org-block (:height 1.25) org-block)))
  )

(defun erasmo-org--presentation-end ()
  ;; Show the mode line again
  (hide-mode-line-mode 0)

  ;; If you use face-remapping-alist, this clears the scaling:
  (setq-local face-remapping-alist '((default variable-pitch default))))

(use-package org-tree-slide
  :hook ((org-tree-slide-play . erasmo-org--presentation-setup)
         (org-tree-slide-stop . erasmo-org--presentation-end))
  :custom
  (org-tree-slide-slide-in-effect t)
  (org-tree-slide-activate-message "Presentation started!")
  (org-tree-slide-deactivate-message "Presentation finished!")
  (org-tree-slide-header t)
  (org-tree-slide-breadcrumbs " > ")
  (org-image-actual-width nil))

;; export----------
(require 'ox-beamer)                    ; latex slides


;; keybind----------
(erasmo-keybind-leader-key-def
  "oi" '(:ignore t :which-key "insert")
  "oil" '(org-insert-link :which-key "insert link")
  "os" '(org-store-link :which-key "store link")
  "on" '(org-toggle-narrow-to-subtree :which-key "toggle narrow")
  "oa" '(org-agenda :which-key "status")
  "oc" '(org-capture t :which-key "capture")
  "ox" '(org-export-dispatch t :which-key "export"))


(provide 'erasmo-org)
