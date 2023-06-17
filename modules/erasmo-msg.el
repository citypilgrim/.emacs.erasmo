;; -*- lexical-binding: t; -*-

;;; particulars

(setq user-full-name erasmo-env-user-full-name)
(setq user-mail-address erasmo-env-user-mail-address)

;;; mail

(use-package mu4e
  :ensure nil
  :defer 20 ; Wait until 20 seconds after startup
  :config

  ;; This is set to 't' to avoid mail syncing issues when using mbsync
  (setq mu4e-change-filenames-when-moving t)

  ;; Refresh mail using isync every 10 minutes
  (setq mu4e-update-interval (* 10 60))
  (setq mu4e-get-mail-command "mbsync -a")
  (setq mu4e-maildir "/mnt/d/mail/94tian@gmail.coma")

  ;; (setq mu4e-drafts-folder "/[Gmail]/Drafts")
  ;; (setq mu4e-sent-folder "/[Gmail]/Sent Mail")
  ;; (setq mu4e-refile-folder "/[Gmail]/All Mail")
  ;; (setq mu4e-trash-folder "/[Gmail]/Trash")

  ;; (setq mu4e-maildir-shortcuts
  ;;       '(("/Inbox" . ?i)
  ;;         ("/[Gmail]/Sent Mail" . ?s)
  ;;         ("/[Gmail]/Trash" . ?t)
  ;;         ("/[Gmail]/Drafts" . ?d)
  ;;         ("/[Gmail]/All Mail" . ?a)))
  )

;;; IRC

(require 'erc)

;; Load authentication info from an external source.  Put sensitive
;; passwords and the like in here.
;; (load "~/.emacs.d/.erc-auth")

;; This is an example of how to make a new command.  Type "/uptime" to
;; use it.
(defun erc-cmd-UPTIME (&rest ignore)
  "Display the uptime of the system, as well as some load-related
     stuff, to the current ERC buffer."
  (let ((uname-output
         (replace-regexp-in-string
          ", load average: " "] {Load average} ["
          ;; Collapse spaces, remove
          (replace-regexp-in-string
           " +" " "
           ;; Remove beginning and trailing whitespace
           (replace-regexp-in-string
            "^ +\\|[ \n]+$" ""
            (shell-command-to-string "uptime"))))))
    (erc-send-message
     (concat "{Uptime} [" uname-output "]"))))

;; Interpret mIRC-style color commands in IRC chats
(setq erc-interpret-mirc-color t)

;; Kill buffers for channels after /part
(setq erc-kill-buffer-on-part t)
;; Kill buffers for private queries after quitting the server
(setq erc-kill-queries-on-quit t)
;; Kill buffers for server messages after quitting the server
(setq erc-kill-server-buffer-on-quit t)

;; enable autojoining
(erc-autojoin-mode 1)

;; alist has the form (SERVER . CHANNELS)
(setq erc-autojoin-channels-alist
      '(("Libera.Chat" "#emacs" "#erc")
        (".*\\.freenode.net" . '("#emacs" "#gnu"))))

;; track notifications
(erc-track-mode t)

;; ignore certain notifications
(setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
                                "324" "329" "332" "333" "353" "477"))
(setq erc-hide-list '("JOIN" "PART" "QUIT" "NICK"))

  ;; disable logging
(customize-set-variable 'erc-log-channels-directory nil)

;;; telegram
;; installed via guix


;;; news
(use-package gnus
  :custom
  (gnus-select-method '(nntp "news.eternal-september.org")))

(provide 'erasmo-msg)
