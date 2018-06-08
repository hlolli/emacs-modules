
;; Show the rgb color defined in hex
(defvar hexcolour-keywords
  '(("#[abcdef[:digit:]]\\{6\\}"
     (0 (put-text-property (match-beginning 0)
                           (match-end 0)
                           'face (list :background 
                                       (match-string-no-properties 0)))))))

(defun hexcolour-add-to-font-lock ()
  (font-lock-add-keywords nil hexcolour-keywords))

(add-hook 'css-mode-hook #'hexcolour-add-to-font-lock)
(add-hook 'scss-mode-hook #'hexcolour-add-to-font-lock)

(defun delete-completion-window-buffer (&optional output)
  "Function for auto-deleteing window buffer."
  (interactive "P") 
  (dolist (win (window-list))                                                                                  
    (when (string= (buffer-name (window-buffer win)) "*Completions*")
      (delete-window win)
      (kill-buffer "*Completions*"))
    (when (string= (buffer-name (window-buffer win)) "*Ido Completions*")
      (delete-window win)
      (kill-buffer "*Ido Completions*")))
  output)                                                                                                      

(add-hook 'comint-preoutput-filter-functions 'delete-completion-window-buffer)


;; From: https://gist.github.com/plexus/5418819323afb892b481816745be15e0
;; Rename clj/cljs/cljc buffers to their namespace name, so you see
;; `foo.bar.core' in the modeline, rather than `core.clj'

(advice-add 'rename-buffer :around #'plexus/clj-ns--rename-buffer-advice)
(defun plexus/clj-ns--rename-buffer-advice (rb-fun newname &optional unique &rest args)
  (let ((filename (buffer-file-name (current-buffer)))
        (buf-start (buffer-substring-no-properties (point-min) (point-min))))
    (if (and filename
             (string-match "\\.clj[cxs]?$" filename)
             (string-match "(ns \\([^\n )]+\\)" buf-start))
        (match-string-no-properties 1 buf-start)
      (apply rb-fun newname unique args))))


(advice-add 'create-file-buffer :around #'plexus/clj-ns--create-file-buffer-advice)
(defun plexus/clj-ns--create-file-buffer-advice (cfb-fun filename &rest args)
  (if (and (file-exists-p filename) (not (file-directory-p filename)))
      (with-temp-buffer
        (insert-file-contents filename)
        (let ((buf-start (buffer-substring-no-properties (point-min) (point-max))))
          (if (and filename
                   (string-match "\\.clj[cxs]?$" filename)
                   (string-match "(ns \\([^\n )]+\\)" buf-start))
              (let ((name (match-string-no-properties 1 buf-start)))
                (generate-new-buffer name)
                name)
            (apply cfb-fun filename args))))
    (apply cfb-fun filename args)))
