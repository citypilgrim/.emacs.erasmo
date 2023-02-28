;; -*- lexical-binding: t; -*-

(use-package geiser
  :init
  (setq geiser-default-implementation 'guile)
  (setq geiser-active-implementations '(guile))
  ;; (setq geiser-guile-binary "/root/.guix-profile/bin/guile")
  ;; (setq geiser-repl-default-port 44555) ; For Gambit Scheme
  (setq geiser-repl-skip-version-check-p 't)
  (setq geiser-implementations-alist '(((regexp "\\.scm$") guile)))
  )

(use-package geiser-guile)

(with-eval-after-load 'geiser-guile
  (add-to-list 'geiser-guile-load-path "~/src/guix"))

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
