;; -*- lexical-binding: t; -*-

;; columne numbering in modeline
(column-number-mode)

;; Enable line numbers for some modes
(dolist (mode '(text-mode-hook
                prog-mode-hook
                conf-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 1))))

;; Override some modes which derive from the above
(dolist (mode '(org-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; text wrapping
(add-hook 'prog-mode-hook #'(lambda () (toggle-truncate-lines 1)))

;; linting
(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode))

;; lanugage specific settings
(add-hook 'python-mode-hook
          (lambda ()
            (setq indent-tabs-mode nil
                  tab-width 4
                  python-indent-offset 4
                  flycheck-flake8-maximum-line-length 120)))

;; python uses virtual environments to sandbox it's dependencies
;; activate the correct virtual encironment with
;; M-x pyvenv-activate RET /path/to/env
(use-package pyvenv
  :hook ((python-mode . pyvenv-mode)
         (python-mode . pyvenv-tracking-mode))
  :config
  ;; Set correct Python interpreter
  (setq pyvenv-post-activate-hooks
        (list (lambda ()
                (setq python-shell-interpreter (concat pyvenv-virtual-env "bin/python3")))))
  (setq pyvenv-post-deactivate-hooks
        (list (lambda ()
                (setq python-shell-interpreter "python3")))))

(add-hook 'js-mode-hook
          (lambda ()
            (setq indent-tabs-mode nil
                  tab-width 2)))

(use-package groovy-mode
  :init
  (add-to-list 'auto-mode-alist '("\\build.gradle\\'" . groovy-mode)))

(use-package dockerfile-mode)

;; language server support
(use-package eglot
  :custom
  (eglot-autoshutdown t)
  :init
  (add-to-list 'eglot-server-programs '(python-mode . ("python-lsp-server"))
               ;; installed via npm
               (add-to-list 'eglot-server-programs '(js-mmode . ("typescript-language-server --stdio")))))

(defun erasmo-ide--add-eglot-hooks (mode-list)
  "Iterates over MODE-LIST recursively to add eglot-ensure to
existing mode hooks.

The mode must be loaded, ie. found with `fboundp'. A mode which
is not loaded will not have a hook added, in which case add it
manually with something like this:

`(add-hook 'some-mode-hook #'eglot-ensure)'
"
  (dolist (mode-def mode-list)
    (let ((mode (if (listp mode-def) (car mode-def) mode-def)))
      (cond
       ((listp mode) (erasmo-ide--add-eglot-hooks mode))
       (t
        (when (and (fboundp mode)
                   (not (eq 'clojure-mode mode)) ; prefer cider
                   (not (eq 'lisp-mode mode))    ; prefer sly/slime
                   (not (eq 'scheme-mode mode))  ; prefer geiser
                   )
          (let ((hook-name (concat (symbol-name mode) "-hook")))
            (message (concat "adding eglot to " hook-name))
            (add-hook (intern hook-name) #'eglot-ensure))))))))

;; add eglot to existing programming modes when eglot is loaded.
(with-eval-after-load "eglot"
  (erasmo-ide--add-eglot-hooks eglot-server-programs))

;; language server support, lsp for java
;; TODO incoporate lsp mode




(provide 'erasmo-ide)
