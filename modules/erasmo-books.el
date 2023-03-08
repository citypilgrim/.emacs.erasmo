;; -*- lexical-binding: t; -*-

;; access the bible with dtk
;; pre-requsites
;; 1. diatheke is installed
;; 2. SWORD_PATH points to where modules are kept
(use-package dtk
  :custom
  (dtk-module "ASV")
  (dtk-module-category "Biblical Texts")
  (dtk-word-wrap t)
  :config
  (setenv "SWORD_PATH" "/sword")
  :init
  (erasmo-keybind-leader-key-def "si"
    '(dtk-bible :which-key "insert passage")))

;; epub reading
(use-package nov
  :init
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode)))

;; processing citations
(use-package citar
  :after org
  :no-require
  :custom
  (org-cite-global-bibliography `(,erasmo-env-biblio))
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
  (citar-bibliography org-cite-global-bibliography)
  (citar-symbols
   `((file ,(all-the-icons-faicon "file-o" :face 'all-the-icons-green :v-adjust -0.1) . " ")
     (note ,(all-the-icons-material "speaker_notes" :face 'all-the-icons-blue :v-adjust -0.3) . " ")
     (link ,(all-the-icons-octicon "link" :face 'all-the-icons-orange :v-adjust 0.01) . " ")))
  (citar-symbol-separator "  ")
  ;; optional: org-cite-insert is also bound to C-c C-x C-@
  :bind
  (:map org-mode-map :package org ("C-c b" . #'org-cite-insert)))

(use-package citar-embark
  :diminish citar-embark-mode
  :after citar embark
  :no-require
  :config (citar-embark-mode))



(provide 'erasmo-books)
