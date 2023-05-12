;; -*- lexical-binding: t; -*-

;; settings for default browser, ungoogled-chromium
(use-package browse-url
  :custom
  (browse-url-chrome-arguments '("--no-sandbox"))
  (browse-url-chromium-arguments '("--no-sandbox")))

;; RESTful queries
(use-package restclient)


(provide 'erasmo-web)
