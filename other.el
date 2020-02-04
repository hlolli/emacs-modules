
;; Show the rgb color defined in hex
(defvar hexcolour-keywords
  '(("#[abcdef[:digit:]]\\{6\\}"
     (0 (put-text-property (match-beginning 0)
                           (match-end 0)
                           'face (list :background
                                       (match-string-no-properties 0)))))))

(defun hexcolour-add-to-font-lock ()
  (interactive)
  (font-lock-add-keywords nil hexcolour-keywords))

(add-hook 'css-mode-hook #'hexcolour-add-to-font-lock)
(add-hook 'scss-mode-hook #'hexcolour-add-to-font-lock)
(add-hook 'web-mode-hook #'hexcolour-add-to-font-lock)
(add-hook 'typescript-mode-hook #'hexcolour-add-to-font-lock)

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

;; From simple emacs
;; https://github.com/Compro-Prasad/simple-emacs/blob/master/simple-shell.el
(defun simple-emacs/init-eshell ()
  "Initialize shell"
  ;; This is an eshell alias
  (defun eshell/clear ()
    (let ((inhibit-read-only t))
      (erase-buffer)))
  ;; This is a key-command
  (defun simple-emacs/eshell-clear-keystroke ()
    "Allow for keystrokes to invoke eshell/clear"
    (interactive)
    (eshell/clear)
    (eshell-send-input))
  ;; Caution! this will erase buffer's content at C-l
  (define-key eshell-mode-map (kbd "C-l") 'simple-emacs/eshell-clear-keystroke)
  (define-key eshell-mode-map (kbd "C-d") 'eshell-delchar-or-maybe-eof))

(add-hook 'eshell-mode-hook 'simple-emacs/init-eshell)


(require 'recentf)
(setq recentf-max-saved-items 200
      recentf-max-menu-items 15)
(recentf-mode +1)

;; Delete trailing whitespace on save
(add-hook 'before-save-hook #'delete-trailing-whitespace nil nil)

;; Enable mouse when in terminal mode
(when (eq window-system nil)
  (xterm-mouse-mode t))
