;; -*- lexical-binding: t; -*-

;; note taking with org-roam
(use-package org-roam
  :after org
  :init
  (setq org-roam-v2-ack t)
  (setq emacsql-sqlite-executable (locate-file "emacsql-sqlite" exec-path))
  ;; defining variables here so that org-agenda and org-capture can initialise
  (customize-set-variable 'org-roam-directory erasmo-env-org-roam-directory)
  (customize-set-variable 'org-roam-dailies-directory erasmo-env-org-roam-dailies-directory)
  :custom
  (org-roam-completion-everywhere nil)
  :bind (
         ("C-c n b" . erasmo-notes-find-node-in-small-buffer-vertically)
         ("C-c n B" . erasmo-notes-find-node-in-small-buffer-horizontally)
         ("C-c n c" . org-roam-capture)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n l" . org-roam-buffer-toggle)
         ("C-c n t" . org-roam-tag-add)
         :map org-roam-dailies-map
         ("Y" . org-roam-dailies-capture-yesterday)
         ("T" . org-roam-dailies-capture-tomorrow))
  :bind-keymap
  ("C-c n d" . org-roam-dailies-map)
  :config
  (require 'org-roam-dailies) ;; Ensure the keymap is available
  (org-roam-db-autosync-mode)
  (setq org-roam-node-display-template
        (concat "${type:15} ${title:*} " (propertize "${tags:10}" 'face 'org-tag))))

;; redefining function for org-open-at-point
(defun erasmo-notes--visit-org-roam-dailies-at-point (id _)
  (condition-case err
      (org-roam-id-open id nil)
    (error
     (embark-org-copy-link-description)
     (let ((file (concat erasmo-env-org-roam-dailies-directory (car kill-ring) ".org")))
       (if (file-exists-p file)
           (find-file file)
         (error (error-message-string err)))))))
(add-hook 'after-init-hook
          (lambda () (org-link-set-parameters "id" :follow #'erasmo-notes--visit-org-roam-dailies-at-point)))

(erasmo-keybind-leader-key-def
  "nd" '(:ignore t :which-key "dailies"))

;; slipbox method as inspired by the author himself, Jethro
(setq org-roam-capture-templates
      '(("m" "main" plain
         "%?"
         :if-new (file+head "main/${slug}.org"
                            "#+title: ${title}\n#+filetags: :draft:\n")
         :immediate-finish t
         :unnarrowed t)
        ("r" "reference" plain "%?"
         :if-new
         (file+head "reference/${title}.org"
                    "#+title: ${title}\n#+filetags: :draft:\n")
         :immediate-finish t
         :unnarrowed t)
        ("a" "article" plain "%?"
         :if-new
         (file+head "articles/${title}.org"
                    "#+title: ${title}\n#+filetags: :draft:\n")
         :immediate-finish t
         :unnarrowed t)
        ("s" "secret" plain "%?"
         :if-new
         (file+head "secrets/${title}.org"
                    "#+title: ${title}\n#+filetags: :draft:\n")
         :immediate-finish t
         :unnarrowed t)))

;; displaying categories
(with-eval-after-load 'org-roam
  (cl-defmethod org-roam-node-type ((node org-roam-node))
    "Return the TYPE of NODE."
    (condition-case nil
        (file-name-nondirectory
         (directory-file-name
          (file-name-directory
           (file-relative-name (org-roam-node-file node) org-roam-directory))))
      (error ""))))

(eval-after-load 'citar
  (defun erasmo-notes-org-roam-node-from-cite (keys-entries)
    (interactive (list (citar-select-ref :filter nil)))
    (let ((title (citar-format--entry
                  "${author editor} :: ${title}"
                  (citar-get-entry keys-entries))))
      (org-roam-capture- :templates
                         '(("r" "reference" plain "%?" :if-new
                            (file+head "reference/${citekey}.org"
                                       ":PROPERTIES:
:ROAM_REFS: [cite:@${citekey}]
:END:
#+title: ${title}\n")
                            :immediate-finish t
                            :unnarrowed t))
                         :info (list :citekey keys-entries)
                         :node (org-roam-node-create :title title)
                         :props '(:finalize find-file)))))

(erasmo-keybind-leader-key-def
 "nr" '(erasmo-notes-org-roam-node-from-cite :which-key "new reference")
 "nT" '((lambda (ref) (interactive "sRef: ") (org-roam-ref-add ref)) :which-key "org-roam-ref-add"))

;; visualising note network on localhost:35901
(use-package org-roam-ui
  :after org-roam
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))


;; workflow----------
;; When I come across a certain task that I would like to work on
;; it is a good habit to log my thoughts and progress. I do this my
;; adding a header for the task in my dailies.
;; The short key for this command is `C-c M-W'
(defun erasmo-notes-org-refile-copy-to-dailies ()
  "Copies the stateless header of subtree into today's dailies, leaving behind
  a time log entry that is a link to the dailies' file."
  (interactive)
  (with-current-buffer (current-buffer)
    ;; (save-excursion)
    ;; adding timestamp
    (org-show-subtree)
    (let* ((start (point))
           (_ (org-goto-first-child))
           (bound (point)))
      (if (= start bound)               ;has no children
          (progn
            (setq bound (org-end-of-subtree))
            (org-back-to-heading))
        (outline-up-heading 1))
      (if (search-forward "- Timelog:" bound t) ;found existing timelogs
          (progn (org-open-line 1) (next-line)
                 (insert "  + ")
                 (erasmo-notes--org-roam-node-insert (erasmo-notes--retrieve-today)))

        ;; no timelogs found
        (org-end-of-line)
        (org-open-line 1) (next-line)
        (insert "- Timelog:")
        (org-open-line 1) (next-line)
        (insert "  + ")
        (erasmo-notes--org-roam-node-insert (erasmo-notes--retrieve-today))))

    ;; creating header at notes
    (org-copy-subtree)
    (let ((tags (org-get-tags))
          (buffer (current-buffer)))
      (org-roam-dailies-goto-today)
      (goto-char (point-max))
      (org-paste-subtree 1)
      (let ((point (point)))
        (org-mark-subtree)
        (forward-line)
        (delete-region (point) (mark))
        (goto-char point)
        (message "Deleted contents of subtree except header"))
      (org-todo 'none)
      (org-set-tags tags)
      (switch-to-buffer buffer))))

(erasmo-keybind-leader-key-def
  "M-W" '(erasmo-notes-org-refile-copy-to-dailies :which-key "org-refile-copy dailies"))



;; utilities----------
(defun erasmo-notes--filter-by-tag (tag-name)
  (lambda (node)
    (member tag-name (org-roam-node-tags node))))

(defun erasmo-notes--list-notes-by-tag (tag-name)
  (delete-dups (mapcar #'org-roam-node-file
                       (seq-filter
                        (erasmo-notes--filter-by-tag tag-name)
                        (org-roam-node-list)))))

(defun erasmo-notes-find-node-in-small-buffer-vertically ()
  "Create a small buffer at the bottom of the frame, and prompts counsel for finding node."
  (interactive)
  (call-interactively 'split-window-below)
  (call-interactively 'other-window)
  (call-interactively 'split-window-below)
  (call-interactively 'delete-window)
  (call-interactively 'other-window)
  (call-interactively 'org-roam-node-find)
  )

(defun erasmo-notes-find-node-in-small-buffer-horizontally ()
  "Create a small buffer at the right of the frame, and prompts counsel for finding node."
  (interactive)
  (call-interactively 'split-window-right)
  (call-interactively 'other-window)
  (call-interactively 'split-window-right)
  (call-interactively 'delete-window)
  (call-interactively 'other-window)
  (call-interactively 'org-roam-node-find)
  )

(defun erasmo-notes--write-roam-group-paths-to-file (FILE ABSOLUTE TAGS)
  "Write the file names of a roam tag group to FILE.

TAGS is a list of strings

If ABSOLUTE is not nil, write the absolute path of the roam note to the FILE.
This function skips over any roam notes that do nott possess an absolute file path.
"
  (let* ((list nil)
         (tags TAGS))
    ;; getting aggregate files from tags
    (while tags
      (setq list (append list (erasmo-notes--list-notes-by-tag (car tags))))
      (setq tags (cdr tags)))
    (setq list (delete-dups list))

    ;; converting paths
    (setq list (mapcar (lambda (arg)
                         (if (file-name-absolute-p arg)
                             (if ABSOLUTE
                                 arg
                               (file-name-nondirectory arg))
                           nil))
                       list))

    ;; removing nils
    (setq list (cl-remove-if (lambda (arg) (eq nil arg)) list))

    ;; writing to file
    (peregrino-elisp-utils/write-list-to-file FILE list)

    (format "Wrote roam groups %s to %s" TAGS FILE)
    ))

(defun erasmo-notes--add-to-roam-file-hooks (FUNCTION &rest ARGS)
  "Adds function to crud event hooks for org roam files only"

  ;; ;; for file saving
  ;; (advice-add
  ;;  #'org-roam-db-autosync--setup-file-h :before
  ;;  (lambda ()
  ;;    (add-hook 'after-save-hook
  ;;              (lambda ()
  ;;                (and org-roam-db-update-on-save
  ;;                     (org-roam-file-p)
  ;;                     (funcall FUNCTION)))
  ;;              nil t)))

  ;; for file saving
  (advice-add
   #'org-roam-db-autosync--setup-file-h :before
   (lambda ()
     (and org-roam-db-update-on-save
          (org-roam-file-p)
          (apply FUNCTION ARGS))))

  ;; for file renaming
  (advice-add
   #'org-roam-db-autosync--rename-file-a :after
   (lambda (old-file new-file-or-dir &rest _args)
     (let ((new-file (if (directory-name-p new-file-or-dir)
                         (expand-file-name (file-name-nondirectory old-file) new-file-or-dir)
                       new-file-or-dir)))
       (setq new-file (expand-file-name new-file))
       (when (org-roam-file-p new-file)
         (apply FUNCTION ARGS)))))

  ;; for file deleting
  (advice-add
   #'org-roam-db-autosync--delete-file-a :after
   (lambda (file &optional _recursive _trash)
     (when (and (not (auto-save-file-name-p file))
                (not (backup-file-name-p file))
                (org-roam-file-p file))
       (apply FUNCTION ARGS))))
  )

(defun erasmo-notes--retrieve-today (&optional directory)
  "Retrieves the file path of today's dailies.

If directory is not null, it returns the absolute path of the file,
assuming the file exists within the directory."
  (let* ((node (org-read-date t nil "now")))
    (if directory
        (concat (file-name-as-directory directory) node ".org")
      node)))

(defun erasmo-notes--org-roam-node-insert (title &optional filter-fn &key templates info)
  "Like org-roam-node-insert, but without prompt"
  (cl-flet ((org-roam-node-read (lambda (&optional arg1 arg2 ) (org-roam-node-from-title-or-alias title))))
    (unwind-protect
        ;; Group functions together to avoid inconsistent state on quit
        (atomic-change-group
          (let* (region-text
                 beg end
                 (_ (when (region-active-p)
                      (setq beg (set-marker (make-marker) (region-beginning)))
                      (setq end (set-marker (make-marker) (region-end)))
                      (setq region-text (org-link-display-format (buffer-substring-no-properties beg end)))))
                 (node (org-roam-node-read region-text filter-fn))
                 (description (or region-text
                                  (org-roam-node-formatted node))))
            (if (org-roam-node-id node)
                (progn
                  (when region-text
                    (delete-region beg end)
                    (set-marker beg nil)
                    (set-marker end nil))
                  (let ((id (org-roam-node-id node)))
                    (insert (org-link-make-string
                             (concat "id:" id)
                             description))
                    (run-hook-with-args 'org-roam-post-node-insert-hook
                                        id
                                        description)))
              (org-roam-capture-
               :node node
               :info info
               :templates templates
               :props (append
                       (when (and beg end)
                         (list :region (cons beg end)))
                       (list :link-description description
                             :finalize 'insert-link))))))
      (deactivate-mark))))



(provide 'erasmo-notes)
