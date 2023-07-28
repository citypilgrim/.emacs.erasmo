(defun erasmo-util-ls-dirs (dir &optional filter)
  (let ((dirs (directory-files dir t "^[^.]+$" t)))
    (if filter
        (cl-remove-if (lambda (p) (string-match-p filter p)) dirs)
      dirs)))

(provide 'erasmo-util)
