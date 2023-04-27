;; -*- lexical-binding: t; -*-

;; tweaking abbrev
;; abbrev is used for shorthand typing
(use-package emacs
  :hook (abbrev-mode . (lambda () (diminish 'abbrev-mode))))

;; inline completion ui
(use-package corfu
  :init
  (global-corfu-mode 1)
  (corfu-popupinfo-mode 1)
  (eldoc-add-command #'corfu-insert)
  :bind (:map corfu-map
              ("M-p" . corfu-popupinfo-scroll-down)
              ("M-n" . corfu-popupinfo-scroll-up)
              ("M-d" . corfu-popupinfo-toggle))
  :custom
  (corfu-cycle t)                 ; Allows cycling through candidates
  (corfu-auto t)                  ; Enable auto completion
  (corfu-auto-prefix 2)           ; Complete with less prefix keys
  (corfu-auto-delay 0.0)          ; No delay for completion
  (corfu-echo-documentation 0.25) ; Echo docs for current completion option
  )

;; extensions to inline completion
(use-package cape
  :init
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  ;;(add-to-list 'completion-at-point-functions #'cape-history)
  ;;(add-to-list 'completion-at-point-functions #'cape-keyword)
  ;;(add-to-list 'completion-at-point-functions #'cape-tex)
  ;;(add-to-list 'completion-at-point-functions #'cape-sgml)
  ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)
  ;;(add-to-list 'completion-at-point-functions #'cape-abbrev)
  ;;(add-to-list 'completion-at-point-functions #'cape-ispell)
  ;;(add-to-list 'completion-at-point-functions #'cape-dict)
  ;;(add-to-list 'completion-at-point-functions #'cape-symbol)
  ;;(add-to-list 'completion-at-point-functions #'cape-line)
  )

;; Silence the pcomplete capf, no errors or messages!
;; Important for corfu
(advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)

;; Ensure that pcomplete does not write to the buffer
;; and behaves as a pure `completion-at-point-function'.
(advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify)
(add-hook 'eshell-mode-hook
          (lambda () (setq-local corfu-quit-at-boundary t
                                 corfu-quit-no-match t
                                 corfu-auto nil)
            (corfu-mode)))

;; minimalist vertical completion UI based on the default completion system
;; Enable vertico
(use-package vertico
  :init
  (vertico-mode)

  ;; Different scroll margin
  ;; (setq vertico-scroll-margin 0)

  ;; Show more candidates
  ;; (setq vertico-count 20)

  ;; Grow and shrink the Vertico minibuffer
  ;; (setq vertico-resize t)

  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  (setq vertico-cycle t))

;; completion style that matches candidates in any order
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;; adds info to the minibuffer completions side
(use-package marginalia
  :custom
  (marginalia-annotators
   '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init (marginalia-mode 1))

;; templates
;; Configure Tempel
(use-package tempel
  ;; Require trigger prefix before template name when completing.
  ;; :custom
  ;; (tempel-trigger-prefix "<")

  :bind (("M-+" . tempel-complete) ;; Alternative tempel-expand
         ("M-*" . tempel-insert))

  :init

  ;; Setup completion at point
  (defun tempel-setup-capf ()
    ;; Add the Tempel Capf to `completion-at-point-functions'.
    ;; `tempel-expand' only triggers on exact matches. Alternatively use
    ;; `tempel-complete' if you want to see all matches, but then you
    ;; should also configure `tempel-trigger-prefix', such that Tempel
    ;; does not trigger too often when you don't expect it. NOTE: We add
    ;; `tempel-expand' *before* the main programming mode Capf, such
    ;; that it will be tried first.
    (setq-local completion-at-point-functions
                (cons #'tempel-expand
                      completion-at-point-functions)))

  (add-hook 'prog-mode-hook 'tempel-setup-capf)
  (add-hook 'text-mode-hook 'tempel-setup-capf)

  ;; Optionally make the Tempel templates available to Abbrev,
  ;; either locally or globally. `expand-abbrev' is bound to C-x '.
  ;; (add-hook 'prog-mode-hook #'tempel-abbrev-mode)
  ;; (global-tempel-abbrev-mode)
)

;; Optional: Add tempel-collection.
;; The package is young and doesn't have comprehensive coverage.
(use-package tempel-collection)

(use-package yasnippet
  :hook ((prog-mode . yas-minor-mode)
         (eshell-mode . yas-minor-mode)
         (yas-minor-mode . (lambda () (diminish 'yas-minor-mode))))
  :config
  (yas-reload-all))

(use-package yasnippet-snippets
  :after yasnippet
  :custom
  (yas-snippet-dirs `(,(concat user-emacs-directory "snippets")
                      yasnippet-snippets-dir)))

;; provides practical commands
(use-package consult
  :bind (("C-s" . consult-line)
         ("C-M-j" . consult-buffer)))

(use-package emacs
  :bind (("C-r" . consult-history))
  :init
  (setq completion-in-region-function #'consult-completion-in-region)
  )

;; discovering keybindings with embark
(require 'embark)
(global-set-key [remap describe-bindings] #'embark-bindings)
(global-set-key (kbd "C-.") 'embark-act)
;; Use Embark to show bindings in a key prefix with `C-h`
(setq prefix-help-command #'embark-prefix-help-command)

(provide 'erasmo-completion)
