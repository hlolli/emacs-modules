
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
  (interactive)                                                                                                
  (dolist (win (window-list))                                                                                  
    (when (string= (buffer-name (window-buffer win)) "*Completions*")
      (delete-window win)
      (kill-buffer "*Completions*"))
    (when (string= (buffer-name (window-buffer win)) "*Ido Completions*")
      (delete-window win)
      (kill-buffer "*Ido Completions*")))
  output)                                                                                                      

(add-hook 'comint-preoutput-filter-functions 'delete-completion-window-buffer)
