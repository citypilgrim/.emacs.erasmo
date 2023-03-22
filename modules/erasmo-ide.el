;; -*- lexical-binding: t; -*-

;; columne numbering in modeline
(column-number-mode)

;; tweaking eldoc
;; eldoc shows you in the mini buffer the argument list
;; of a function call you are writing.
;; it supports elisp and a few other languages
(use-package eldoc
  :hook (eldoc-mode . (lambda () (diminish 'eldoc-mode))))

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
  (global-flycheck-mode)
  (diminish 'flycheck-mode))

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
(defun erasmo-ide--lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook ((lsp-mode . erasmo-ide--lsp-mode-setup)
         (lsp-lens-mode . (lambda () (diminish 'lsp-lens-mode))))
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (lsp-enable-which-key-integration t)
  (global-set-key (kbd "C-M-;") 'lsp-workspace-folders-open))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'bottom))

;; workspace managemet
;; Try these commands with =M-x=:
;; - ~lsp-treemacs-symbols~ - Show a tree view of the symbols in the current file
;; - ~lsp-treemacs-references~ - Show a tree view for the references of the symbol under the cursor
;; - ~lsp-treemacs-error-list~ - Show a tree view for the diagnostic messages in the project

(use-package lsp-treemacs
  :after lsp-mode
  :config
  (erasmo-keybind-leader-key-def :prefix lsp-keymap-prefix
    "t" '(:ignore t :which-key "treemacs")
    "te" '(lsp-treemacs-error-list :which-key "errors")
    "tr" '(lsp-treemacs-references :which-key "refs")
    "ts" '(lsp-treemacs-symbols :which-key "symbols")
    "tw" '(treemacs :which-key "workspace"))
  (global-set-key (kbd "C-M-'") 'treemacs-switch-workspace)
  :custom
  (lsp-treemacs-sync-mode t))

(use-package lsp-java
  :hook (java-mode . indent-tabs-mode)
  :init
  (add-hook 'java-mode-hook #'lsp)

  (add-hook 'java-mode-hook (lambda () (setq-local c-basic-offset tab-width)))
  :config
  (require 'dap-java)
  :custom
  (lsp-enable-file-watchers nil)
  (lsp-java-autobuild-enabled t)
  (lsp-java-completion-max-results 50)
  (lsp-java-vmargs '("-XX:+UseParallelGC"
                     "-XX:GCTimeRatio=4"
                     "-XX:AdaptiveSizePolicyWeight=90"
                     "-Dsun.zip.disableMemoryMapping=true"
                     "-Xmx2G"
                     "-Xms100m"))
  (lsp-java-format-settings-url (lsp--path-to-uri (expand-file-name "resource/jdtls/eclipse-java-export.xml" user-emacs-directory)))
  (lsp-java-format-settings-profile "Eclipse [built-in] tabs 8")
  ;; (lsp-java-server-install-dir "/opt/jdtls/test")

  ;; settings for gradle
  (lsp-java-import-gradle-home (getenv "GRADLE_HOME"))
  (lsp-java-import-gradle-java-home (getenv "JAVA_HOME"))
  (lsp-java-import-gradle-user-home "~/.gradle")
  (lsp-java-import-gradle-enabled t)
  (lsp-java-import-gradle-version "7.4.2")
  ;; lsp-java-import-gradle-arguments
  ;; lsp-java-import-gradle-jvm-arguments
  ;; lsp-java-import-gradle-offline-enabled
  ;; lsp-java-import-gradle-wrapper-enabled
  )

(provide 'erasmo-ide)
