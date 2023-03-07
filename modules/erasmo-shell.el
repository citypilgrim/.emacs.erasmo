;; -*- lexical-binding: t; -*-

;; autocomplete
(use-package esh-autosuggest
  :hook (eshell-mode . esh-autosuggest-mode))

;; status on prompt
;; If you want to display the python virtual environment info,
;; you need to install virtualenvwrapper and virtualenvwrapper.el
(use-package eshell-prompt-extras
  :after eshell
  :custom
  (eshell-prompt-function #'epe-theme-dakrone))

(provide 'erasmo-shell)
