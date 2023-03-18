;; -*- lexical-binding: t; -*-

;; TODO do without manual installation of whisper.el
;; requires dependency whisper.cpp to be built
;; building is automatic, but requires linux-libc-dev
;; which is installed outside of guix
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

(provide 'erasmo-ai)
