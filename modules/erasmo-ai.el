;; -*- lexical-binding: t; -*-

;; TODO do without manual installation of whisper.el
;; requires dependency whisper.cpp to be built.
;; Building is automatic, but requires linux-libc-dev
;; which is installed outside of guix.
;; Upon initial start up of whisper, it prompts for
;; building and retrieval of model, the latter
;; requiring internet connection
(add-to-list 'load-path
             (concat user-emacs-directory
                     "site-packages/whisper.el"))
(require 'whisper)
(use-package whisper
  :ensure nil
  :bind ("<f5>" . whisper-run)
  :config
  (setq whisper-install-directory "/tmp/"
        whisper-model "base"
        whisper-language "en"
        whisper-translate nil
        whisper--ffmpeg-input-device "RDPSource"))

;; chatGPT
(use-package gptel
  :custom
  (gptel-api-key erasmo-env-openai-token)
  (gptel-model "gpt-3.5-turbo"))

;; chatGPT + dall-e
(use-package chatgpt-shell
  :custom
  (chatgpt-shell-openai-key erasmo-env-openai-token))

(provide 'erasmo-ai)
