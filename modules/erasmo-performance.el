;; -*- lexical-binding: t; -*-

;; Compile warnings
(setq byte-compile-warnings t)
(setq warning-minimum-level :emergency)
(setq comp-async-report-warnings-errors nil) ;; native-comp warning
(setq byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local))

;; Increasing the amount that emacs can read from process so as to support lsp speed
(setq read-process-output-max (* 1024 1024))

;; dynamic gc threshold
(use-package gcmh
  :diminish gcmh-mode
  :config
  (setq gcmh-idle-delay 5
        gcmh-high-cons-threshold (* 16 1024 1024))  ; 16mb
  (gcmh-mode 1))

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-percentage 0.1) ;; Default value for `gc-cons-percentage'
            (eval-after-load "gcmh" '(diminish 'gcmh-mode))))

(provide 'erasmo-performance)
