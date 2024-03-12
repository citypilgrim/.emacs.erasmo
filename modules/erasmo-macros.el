(defun erasmo-ux-make-region-rectangle (arg)
  "Converts an active region into a rectangular block by padding each
line to fit the longest line plus ARG spaces."
  (interactive "p")
  (if (region-active-p)
      (let* ((begin (region-beginning))
             (end (region-end))
             (lines (split-string (buffer-substring begin end) "\n"))
             (maxwidth (+ arg (apply 'max (mapcar 'string-width lines)))))
        (save-excursion
          (goto-char begin)
          (while (< (point) end)
            (end-of-line)
            (let ((missing (- maxwidth (current-column))))
              (when (> missing 0)
                (insert (make-string missing ?\s))
                (setq end (+ end missing))))
            (forward-line 1))))
    (message "No active region")))

(provide 'erasmo-macros)
