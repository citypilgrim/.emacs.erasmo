;; -*- lexical-binding: t; -*-

;;; mail with gnus

(setq user-full-name erasmo-env-user-full-name)
(setq user-mail-address erasmo-env-user-mail-address)

(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-default-smtp-server erasmo-env-smtpmail-default-smtp-server
      smtpmail-smtp-service erasmo-env-smtpmail-smtp-service
      ;; set the val below only if the server complains
      ;; smtpmail-local-domain "homepc"
      )

(defun erasmo-msg--message-pre-send-check-attachment ()
  "Check attachment before send mail."
  (when t
    (unless
        (y-or-n-p "The message suggests that you may want to attach something, but no attachment is found. Send anyway?")
      (error "It seems that an attachment is needed, but none was found. Aborting sending."))))
(add-hook 'message-send-hook 'erasmo-msg--message-pre-send-check-attachment)

;; (setq epa-file-cache-passphrase-for-symmetric-encryption t) ; ask encryption password once

(add-hook 'message-mode-hook
          '(lambda ()
             (flyspell-mode t)))

(defun erasmo-msg-gnus-group-list-subscribed-groups ()
  "List all subscribed groups with or without un-read messages"
  (interactive)
  (gnus-group-list-all-groups 5))

;; TODO fix eternal september
(use-package gnus
  :defer t
  :custom
  ;; (gnus-select-method '(nntp "news.eternal-september.org"))
  (gnus-select-method erasmo-env-gnus-select-method)
  (gnus-thread-sort-functions '(gnus-thread-sort-by-most-recent-date
                                (not gnus-thread-sort-by-number)))
  ;; (gnus-use-cache t)
  (gnus-read-active file 'some)
  (gnus-summary-thread-gathering-function 'gnus-gather-threads-by-subject)
  (gnus-thread-hide-subtree t)          ;might want to comment this
  (gnus-thread-ignore-subject t)        ;might want to comment this
  :config
  (if erasmo-env-gnus-secondary-select-methods
      (setq gnus-secondary-select-methods ;use append if this is not nil
            erasmo-env-gnus-secondary-select-methods))
  :init
  (setq gnus-use-correct-string-widths nil)
  (add-hook 'gnus-group-mode-hook 'gnus-topic-mode)

  ;; organizing the mail folders
  (eval-after-load 'gnus-topic
    '(progn
       (setq gnus-message-archive-group '((format-time-string "sent.%Y")))
       (setq gnus-server-alist '(("archive" nnfolder "archive" (nnfolder-directory "~/Mail/archive")
                                  (nnfolder-active-file "~/Mail/archive/active")
                                  (nnfolder-get-new-mail nil)
                                  (nnfolder-inhibit-expiry t))))

       ;; "Gnus" is the root folder, and the added mail accounts
       (setq gnus-topic-topology erasmo-env-gnus-topic-topologies)

       ;; each topic corresponds to a public imap folder
       (setq gnus-topic-alist erasmo-env-gnus-topic-alist))))

(use-package bbdb                       ;for address look up
  :after gnus
  :hook
  (message-mode . (lambda () (local-set-key (kbd "TAB") 'bbdb-complete-name)))
  (gnus-startup . bbdb-insinuate-gnus)
  :init
  (bbdb-initialize 'message 'gnus 'sendmail))


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
;; installed via guix with emacs-telega


(provide 'erasmo-msg)
