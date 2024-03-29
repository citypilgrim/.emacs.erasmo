;; -*- lexical-binding: t; -*-

;; column numbering in modeline
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

;; paranthesis management
(add-hook 'prog-mode-hook (lambda () (electric-pair-mode)))

;; linting
(use-package flycheck
  :init
  (global-flycheck-mode)
  (diminish 'flycheck-mode))

;;; cmake
(use-package cmake-mode)

(use-package cmake-project)
;; activate mode when detected CMakeLists.txt within project dir
(defun maybe-cmake-project-mode ()
  (if (or (file-exists-p "CMakeLists.txt")
          (file-exists-p (expand-file-name "CMakeLists.txt" (car (project-roots (project-current))))))
      (cmake-project-mode)))

;;; structural editing

;; structural editing with tree-sitter
(use-package tree-sitter
  :init
  (setq treesit-language-source-alist
        '((bash "https://github.com/tree-sitter/tree-sitter-bash")
          (cmake "https://github.com/uyha/tree-sitter-cmake")
          (c++ "https://github.com/tree-sitter/tree-sitter-cpp.git")
          (c "https://github.com/tree-sitter/tree-sitter-c.git")
          (css "https://github.com/tree-sitter/tree-sitter-css")
          (elisp "https://github.com/Wilfred/tree-sitter-elisp")
          (go "https://github.com/tree-sitter/tree-sitter-go")
          (html "https://github.com/tree-sitter/tree-sitter-html")
          (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
          (rust "https://github.com/hydro-project/rust-sitter.git")
          (json "https://github.com/tree-sitter/tree-sitter-json")
          (java "https://github.com/tree-sitter/tree-sitter-java.git")
          (make "https://github.com/alemuller/tree-sitter-make")
          (markdown "https://github.com/ikatyang/tree-sitter-markdown")
          (python "https://github.com/tree-sitter/tree-sitter-python")
          (toml "https://github.com/tree-sitter/tree-sitter-toml")
          (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
          (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
          (yaml "https://github.com/ikatyang/tree-sitter-yaml")))
  (setq treesit-load-name-override-list '((rust "rust-sitter" "tree_sitter_rust"))))

(setq major-mode-remap-alist
      '(
        (c++-mode . c++-ts-mode)
        (python-mode . python-ts-mode)
        ))

;; to install
;; (mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist))


;;; language server

;; eglot
(use-package eglot
  :custom
  (eglot-autoshutdown t)
  :config
  (add-to-list 'eglot-server-programs '(python-mode . ("python-lsp-server"))
               ;; installed via npm
               (add-to-list 'eglot-server-programs '(js-mode . ("typescript-language-server --stdio")))))

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
(setq sh-mode-hook (cdr sh-mode-hook))  ;no lsp for sh mode yet


;; lsp
(defun erasmo-ide--lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode)
  (diminish 'abbrev-mode)
  (diminish 'company-mode)
  (diminish 'company-capf)
  (corfu-mode -1))

(use-package lsp-mode
  :after tree-sitter
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

;;; java
(use-package lsp-java
  :hook (java-mode . indent-tabs-mode)
  :init
  (add-hook 'java-mode-hook #'lsp)
  (add-hook 'java-mode-hook (lambda () (setq-local c-basic-offset tab-width)))
  (remove-hook 'java-mode-hook #'eglot-ensure)
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
  ;; (lsp-java-import-gradle-home "/opt/gradle/gradle-7.4.2")
  ;; (lsp-java-import-gradle-java-home "/gnu/store/mxpgwmjbrq3qzx3asg27hf60i56b0f76-openjdk-11.0.17")
  (lsp-java-import-gradle-user-home "~/.gradle")
  (lsp-java-import-gradle-enabled t)
  ;; (lsp-java-import-gradle-version "7.4.2")
  (lsp-java-import-gradle-wrapper-enabled t)
  ;; lsp-java-import-gradle-arguments
  ;; lsp-java-import-gradle-jvm-arguments
  ;; lsp-java-import-gradle-offline-enabled
  )

;;; c/c++ programming
(defun erasmo-ide--setup-cc-mode ()
  (interactive)
  (lsp-deferred)
  (require 'dap-cpptools)
  (dap-cpptools-setup)
  ;; (maybe-cmake-project-mode)
  )

(use-package cc-mode
  :hook
  ((c-mode . erasmo-ide--setup-cc-mode)
   (c-ts-mode . erasmo-ide--setup-cc-mode)
   (c++-mode . erasmo-ide--setup-cc-mode)
   (c++-ts-mode . erasmo-ide--setup-cc-mode)))

;;; python
(use-package lsp-pyright
  :ensure t
  :hook (python-ts-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp-deferred))))  ; or lsp-deferred

(add-hook 'python-ts-mode-hook
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
;;; javascrpit
(add-hook 'js-mode-hook
          (lambda ()
            (setq indent-tabs-mode nil
                  tab-width 2)))
;;; groovy
(use-package groovy-mode
  :init
  (add-to-list 'auto-mode-alist '("\\build.gradle\\'" . groovy-mode)))

;;; docker
(use-package dockerfile-mode)

;; debugging with gdb
(use-package gdb-mi
  :custom
  (gdb-many-windows t)
  (gdb-show-main t))

;; coverage
;; cov-mode runs the profiling mode, i.e. demarking coverage with number of calls
;; setting cov-coverage-mode to t before activating cov-mode activates coverage
;; mode, which shows either cover or no cover
(use-package cov)


(provide 'erasmo-ide)
