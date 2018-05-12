
(defun comment-or-uncomment-region-or-line ()
  "Comments or uncomments the region or the current line if there's no active region."
  (interactive)
  (let (beg end)
    (if (region-active-p)
        (setq beg (region-beginning) end (region-end))
      (setq beg (line-beginning-position) end (line-end-position)))
    (comment-or-uncomment-region beg end)))

;; Vim style increment
(defun increment-number-decimal (&optional arg)
  "Increment the number forward from point by 'arg'."
  (interactive "p*")
  (save-excursion
    (save-match-data
      (let (inc-by field-width answer)
        (setq inc-by (if arg arg 1))
        (skip-chars-backward "0123456789")
        (when (re-search-forward "[0-9]+" nil t)
          (setq field-width (- (match-end 0) (match-beginning 0)))
          (setq answer (+ (string-to-number (match-string 0) 10) inc-by))
          (when (< answer 0)
            (setq answer (+ (expt 10 field-width) answer)))
          (replace-match (format (concat "%0" (int-to-string field-width) "d")
                                 answer)))))))

(defun decrement-number-decimal (&optional arg)
  (interactive "p*")
  (my-increment-number-decimal (if arg (- arg) -2)))

(defun sudo-edit (&optional arg)
  "Edit currently visited file as root.
With a prefix ARG prompt for a file to visit.
Will also prompt for a file to visit if current
buffer is not visiting a file."
  (interactive "P")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:"
                         (ido-read-file-name "Find file(as root): ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))


;; Resize the buffer window if more than 1 is in view
(defun enlarge-window-horizontally--double ()
  (interactive)
  (enlarge-window-horizontally 10))

(defun shrink-window-horizontally--double ()
  (interactive)
  (shrink-window-horizontally 10))

(defun shrink-window--double ()
  (interactive)
  (shrink-window 10))

(defun enlarge-window--double ()
  (interactive)
  (enlarge-window 10))

;; replace the selected region with yank
(defun yank-replace (beg end)
  (interactive "r")
  (delete-region beg end)
  (yank))

(defun indent-buffer () 
  "Indent the currently visited buffer."
  (interactive)
  (indent-region (point-min) (point-max)))

(defun chomp (str)
  "Chomp leading and tailing whitespace from STR."
  (while (string-match "\\`\n+\\|^\\s-+\\|\\s-+$\\|\n+\\'"
                       str)
    (setq str (replace-match "" t t str)))
  str)

(defun chomp-line-or-region ()
  "Remove leading and traling whitespace from current line or region."
  (interactive)
  (let (pos1 pos2 bds)
    (if (region-active-p)
        (setq pos1 (region-beginning) pos2 (region-end))
      (progn
        (setq bds (bounds-of-thing-at-point 'line))
        (setq pos1 (car bds) pos2 (cdr bds))))
    (setq myStr (buffer-substring pos1 pos2))
    (setq myStrChomped (chomp myStr))
    (delete-region pos1 pos2)
    (goto-char pos1)
    (insert myStrChomped)))

(defun echo-active-modes ()
  "Give a message of which minor modes are enabled in the current buffer."
  (interactive)
  (let ((active-modes))
    (mapc (lambda (mode) (condition-case nil
                             (if (and (symbolp mode) (symbol-value mode))
                                 (add-to-list 'active-modes mode))
                           (error nil) ))
          minor-mode-list)
    (message "Active modes are %s" active-modes)))


(defun rename-file-and-buffer ()
  "Rename the current buffer and file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (message "Buffer is not visiting a file!")
      (let ((new-name (read-file-name "New name: " filename)))
        (cond
         ((vc-backend filename) (vc-rename-file filename new-name))
         (t
          (rename-file filename new-name t)
          (set-visited-file-name new-name t t)))))))

(defun delete-file-and-buffer ()
  "Kill the current buffer and deletes the file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (when filename
      (when (yes-or-no-p (format "Are you sure you want to delete? %s" filename))
	(if (vc-backend filename)
	    (vc-delete-file filename)
	  (progn
	    (delete-file filename)
	    (message "Deleted file %s" filename)
	    (kill-buffer)))))))


(defun emacs4art-download-fira-mono-font ()
  "Only for initial start"
  (let* ((font-dest (cl-case window-system
                      (x  (concat (or (getenv "XDG_DATA_HOME")
                                      (concat (getenv "HOME") "/.local/share"))
                                  "/fonts/"))
                      (mac (concat (getenv "HOME") "/Library/Fonts/" ))
                      (ns (concat (getenv "HOME") "/Library/Fonts/" ))))
         (known-dest? (stringp font-dest))
         (font-dest (or font-dest (read-directory-name "Font installation directory: " "~/"))))
    (unless (file-directory-p font-dest) (mkdir font-dest t))
    (url-copy-file "https://github.com/mozilla/Fira/blob/master/ttf/FiraMono-Regular.ttf?raw=true"
                   (expand-file-name "FiraMono-regular.ttf" font-dest) t)
    (when known-dest?
      (message "Fonts downloaded, updating font cache... <fc-cache -f -v> ")
      (shell-command-to-string (format "fc-cache -f -v")))
    (message "Successfully %s `Symbola' fonts to `%s'!"
             (if known-dest? "installed" "downloaded")
             font-dest)))


;; functions.el ends here
