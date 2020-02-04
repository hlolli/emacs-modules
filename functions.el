(require 'json)

(defun hlolli/replace-alist-mode (alist oldmode newmode)
  (dolist (aitem alist)
    (if (eq (cdr aitem) oldmode)
        (setcdr aitem newmode))))

(defun hlolli/comment-or-uncomment-region-or-line ()
  "Comments or uncomments the region or the current line if there's no active region."
  (interactive)
  (let (beg end)
    (if (region-active-p)
        (setq beg (region-beginning) end (region-end))
      (setq beg (line-beginning-position) end (line-end-position)))
    (comment-or-uncomment-region beg end)))

;; Vim style increment
(defun hlolli/increment-number-decimal (&optional arg)
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

(defun hlolli/decrement-number-decimal (&optional arg)
  (interactive "p*")
  (increment-number-decimal (if arg (- arg) -2)))

(defun hlolli/sudo-edit (&optional arg)
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
(defun hlolli/enlarge-window-horizontally--double ()
  (interactive)
  (enlarge-window-horizontally 10))

(defun hlolli/shrink-window-horizontally--double ()
  (interactive)
  (shrink-window-horizontally 10))

(defun hlolli/shrink-window--double ()
  (interactive)
  (shrink-window 10))

(defun hlolli/enlarge-window--double ()
  (interactive)
  (enlarge-window 10))

;; replace the selected region with yank
(defun hlolli/yank-replace (beg end)
  (interactive "r")
  (delete-region beg end)
  (yank))

(defun hlolli/indent-buffer ()
  "Indent the currently visited buffer."
  (interactive)
  (indent-region (point-min) (point-max)))

(defun hlolli/chomp (str)
  "Chomp leading and tailing whitespace from STR."
  (while (string-match "\\`\n+\\|^\\s-+\\|\\s-+$\\|\n+\\'"
                       str)
    (setq str (replace-match "" t t str)))
  str)

(defun hlolli/chomp-line-or-region ()
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

(defun hlolli/echo-active-modes ()
  "Give a message of which minor modes are enabled in the current buffer."
  (interactive)
  (let ((active-modes))
    (mapc (lambda (mode) (condition-case nil
                             (if (and (symbolp mode) (symbol-value mode))
                                 (add-to-list 'active-modes mode))
                           (error nil) ))
          minor-mode-list)
    (message "Active modes are %s" active-modes)))

(defun hlolli/rename-file-and-buffer ()
  "Rename the current buffer and file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (message "Buffer is not visiting a file!")
      (let ((new-name (read-file-name "New name: " filename)))
        (rename-file filename new-name t)
        (set-visited-file-name new-name t t)))))

(defun hlolli/delete-file-and-buffer ()
  "Kill the current buffer and deletes the file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (when filename
      (when (yes-or-no-p (format "Are you sure you want to delete? %s" filename))
	(progn
          (delete-file filename)
          (message "Deleted file %s" filename)
          (kill-buffer))))))

(defun hlolli/move-file (new-location)
  "Write this file to NEW-LOCATION, and delete the old one."
  (interactive (list (expand-file-name
                      (if buffer-file-name
                          (read-file-name "Move file to: ")
                        (read-file-name "Move file to: "
                                        default-directory
                                        (expand-file-name (file-name-nondirectory (buffer-name))
                                                          default-directory))))))
  (when (file-exists-p new-location)
    (delete-file new-location))
  (let ((old-location (expand-file-name (buffer-file-name))))
    (message "old file is %s and new file is %s"
             old-location
             new-location)
    (write-file new-location t)
    (when (and old-location
               (file-exists-p new-location)
               (not (string-equal old-location new-location)))
      (delete-file old-location))))

(defun hlolli/emacs-modules-download-fira-mono-font ()
  "Only for initial start"
  (when (display-graphic-p)
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
	       font-dest))))

(defun hlolli/yank-github-link ()
  "Quickly share a github link of what you are seeing in a buffer. Yanks
a link you can paste in the browser."
  (interactive)
  (let* ((remote (or (magit-get-push-remote) "origin"))
         (url (magit-get "remote" remote "url"))
         (project (if (string-prefix-p "git" url)
                      (substring  url 15 -4)   ;; git link
                    (substring  url 19 -4))) ;; https link
         (link (format "https://github.com/%s/blob/%s/%s#L%d"
                       project
                       (magit-get-current-branch)
                       (magit-current-file)
                       (count-lines 1 (point)))))
    (kill-new link)))

(defun hlolli/prettier-mode--disabled-on-ssh ()
  (when (not (and (eq 'string (type-of (buffer-file-name)))
                  (string-equal
                   "/scp" (substring-no-properties
                           (buffer-file-name)
                           0 4))))
    (prettier-js-mode)
    (run-with-idle-timer 1 nil (lambda () (font-lock-flush)))))

(defun hlolli/camel-to-snake-case-region ()
  (interactive)
  (progn (replace-regexp "\\([A-Z]\\)" "_\\1" nil (region-beginning) (region-end))
         (downcase-region (region-beginning) (region-end))))

(defun hlolli/set-javascript-indent-from-prettier ()
  (interactive)
  (progn
    (let* ((prettier-dir-with-json (locate-dominating-file default-directory ".prettierrc.json"))
           (prettier-dir-wo-json (locate-dominating-file default-directory ".prettierrc"))
           (prettier-json (or (and prettier-dir-with-json (concat prettier-dir-with-json ".prettierrc.json"))
                              (and prettier-dir-wo-json (concat prettier-dir-wo-json ".prettierrc")))))
      (when (and prettier-json (file-exists-p prettier-json))
        (let ((prettier-list (json-read-file prettier-json))
              (index 0)
              (result nil))
          (while (and (< index (length prettier-list))
                      (eq result nil))
            (when (eq 'tabWidth (car (nth index prettier-list)))
              (setq-local result (cdr (nth index prettier-list))))
            (setq-local index (1+ index)))
          (setq js-indent-level result
                js2-basic-offset result
                web-mode-markup-indent-offset result
                web-mode-code-indent-offset result))))))

(defun stop-using-minibuffer ()
  "kill the minibuffer"
  (when (and (>= (recursion-depth) 1) (active-minibuffer-window))
    (abort-recursive-edit)))

(add-hook 'mouse-leave-buffer-hook 'stop-using-minibuffer)

;; functions.el ends here
