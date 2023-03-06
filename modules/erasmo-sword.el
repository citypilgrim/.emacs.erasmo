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


(provide 'erasmo-sword)
