;; -*- lexical-binding: t; -*-

;; spotify integration
(use-package smudge
  :init
  (setq smudge-transport 'connect)
  :config
  (define-key smudge-mode-map (kbd "C-c .") 'smudge-command-map)
  :custom
  (smudge-oauth2-client-id erasmo-env-spotify-client-id)
  (smudge-oauth2-client-secret erasmo-env-spotify-client-secret))

(provide 'erasmo-apps)
