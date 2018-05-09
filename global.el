
;; Built-in modes config
(add-hook 'emacs-lisp-mode-hook #'eldoc-mode)
(add-hook 'emacs-lisp-mode-hook #'paredit-mode)
(add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)
(add-hook 'lisp-interaction-mode-hook #'emacs-lisp-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'eldoc-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'paredit-mode)

;; Disable blinking cursor
(blink-cursor-mode 0)

;; Display the column number
(column-number-mode t)

;; Show the time
(display-time-mode)

;; Write yes as y and no as n
(fset 'yes-or-no-p 'y-or-n-p)

;; Highlight the current line
(global-hl-line-mode t)

;; Display the line number
(line-number-mode t)

;; Hide the menu bar
(menu-bar-mode -1)

;; Save history
(savehist-mode 1)

;; Display the zoom value
(size-indication-mode t)

;; Hide the scrollbar
(toggle-scroll-bar -1)

;; Hide the toolbar
(tool-bar-mode -1)

;; Use ibuffer instead of buffer-list
(global-set-key (kbd "C-x C-b") 'ibuffer)


;; Try to load emacs4art-font it exists
(let ((default-font (if (member emacs4art-font (font-family-list))
			emacs4art-font "DejaVu Sans Mono")))
  (set-face-attribute 'default nil
		      :font default-font
		      :height 120
		      :weight 'normal
		      :width 'normal)
  ;; Enlarge just a tiny bit
  (text-scale-adjust +1))


(setq ac-ignore-case nil
      auto-save-list-file-prefix (concat user-emacs-directory "tmp/auto-save-list/.saves-")
      backup-directory-alist `(("." . ,(concat user-emacs-directory "tmp/autosaves")))
      custom-file (concat user-emacs-directory "tmp/custom.el")
      create-lockfiles nil
      electric-indent-inhibit t
      frame-title-format '((:eval (if (buffer-file-name) (abbreviate-file-name (buffer-file-name)) "%b")))      
      inhibit-startup-message t
      require-final-newline t
      ring-bell-function 'ignore
      savehist-additional-variables '(kill-ring search-ring regexp-search-ring)
      savehist-file (concat user-emacs-directory "tmp/savehist")
      scroll-margin 2 ;; nice scrolling
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

;; globalvars.el ends here