;; -*- lexical-binding: t; -*-

(with-eval-after-load "eglot" (remove-hook 'tex-mode-hook #'eglot-ensure))

(with-eval-after-load 'org
  ;; (customize-set-variable 'org-pretty-entities t) ;pretty math symbols
  (plist-put org-format-latex-options :scale 2) ;show larger preview
  )

(use-package cdlatex
  :hook ((LaTeX-mode . cdlatex-mode)
         (cdlatex-mode . (lambda () (diminish 'cdlatex-mode))))
  :bind (:map org-mode-map
              ("C-<tab>" . cdlatex-tab)
              :map latex-mode-map
              ("<tab>" . cdlatex-tab)))

;; convenience func to get into the mathematical latex feels
;; utilise org-latex-preview to inline render latex formulas
(defun erasmo-latex-setup ()
  (interactive)
  (org-cdlatex-mode)
  (diminish 'org-cdlatex-mode)
  (org-toggle-pretty-entities))


;; vanilla latex mode
(use-package auctex)

;; In ~latex-mode~, you can use the following key bindings:
;; - ~C-c {~ ; insert environment
;; - ~C-c C-m~ ; insert macro
;; - ~C-c C-s~ ; insert section
;; - ~tex-fold-~, ~C-c C-o~ ; folding latex headings
;; - ~preview-~; inline visualisation
(add-hook 'latex-mode-hook
          (lambda ()
            (progn
              (prettify-symbols-mode 1) ;pretty math symbols
              (outline-minor-mode 1)    ;folding
              (cdlatex-mode)

              ;; editing the preview package's function
              ;; to show larger images
              (defun preview-larger-previews ()
                (setq preview-scale-function
                      (lambda () (* 1.25
                                    (funcall (preview-scale-from-face)))))))))


(provide 'erasmo-latex)
