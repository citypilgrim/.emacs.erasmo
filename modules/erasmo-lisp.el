;; -*- lexical-binding: t; -*-

;; tree editing
(use-package lispy
  :hook ((emacs-lisp-mode . lispy-mode)
         (lisp-data-mode . lispy-mode)
         (lisp-mode . lispy-mode)
         (scheme-mode . lispy-mode)
         (lispy-mode . (lambda () (diminish 'lispy-mode)))))

(use-package geiser
  :custom
  (geiser-default-implementation 'guile)
  (geiser-active-implementations '(guile))
  (geiser-repl-skip-version-check-p 't)
  (geiser-implementations-alist '(((regexp "\\.scm$") guile)))
  )

(use-package geiser-guile)

;; Assuming the Guix checkout is in ~/src/guix.
;; Yasnippet configuration
(with-eval-after-load 'yasnippet
  (add-to-list 'yas-snippet-dirs "~/src/guix/etc/snippets/yas"))

;; Tempel configuration
(with-eval-after-load 'tempel
  ;; Ensure tempel-path is a list -- it may also be a string.
  (unless (listp 'tempel-path)
    (setq tempel-path (list tempel-path)))
  (add-to-list 'tempel-path "~/src/guix/etc/snippets/tempel/*"))

(provide 'erasmo-lisp)
