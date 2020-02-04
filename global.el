
;; Built-in modes config
(add-hook 'emacs-lisp-mode-hook #'paredit-mode)
(add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)
(add-hook 'lisp-interaction-mode-hook #'emacs-lisp-mode)
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

;; Auto revert (saved) files when they change on disk
(global-auto-revert-mode t)

;; Try to load emacs-modules-font it exists
(let ((default-font (if (member emacs-modules-font (font-family-list))
                        emacs-modules-font
                      (if (member "PT Mono" (font-family-list))
                          "PT Mono" "Arial"))))
  (unless (member "Fira Mono" (font-family-list))
    (emacs-modules-download-fira-mono-font))
  (set-face-attribute 'default nil
                      :font default-font
                      :height 180
                      :weight 'normal
                      :width 'normal)
  ;; Enlarge just a tiny bit
  (text-scale-adjust +1))


(setq ac-ignore-case nil
      auto-save-list-file-prefix (concat user-emacs-directory "tmp/auto-save-list/.saves-")
      backup-directory-alist `(("." . ,(concat user-emacs-directory "tmp/autosaves")))
      custom-file (concat user-emacs-directory "tmp/custom.el")
      custom-safe-themes t
      create-lockfiles nil
      completion-show-inline-help nil
      completion-auto-help nil
      electric-indent-mode t
      frame-title-format '((:eval
                            (if (buffer-file-name)
                                (abbreviate-file-name (buffer-file-name)) "%b")))
      gc-cons-threshold 20000000
      large-file-warning-threshold 1000000 ;; 1MB
      help-window-select t
      inhibit-startup-message t
      nrepl-use-ssh-fallback-for-remote-hosts t
      nxml-child-indent 4
      nxml-attribute-indent 4
      package-check-signature nil
      require-final-newline t
      ring-bell-function 'ignore
      savehist-additional-variables '(kill-ring search-ring regexp-search-ring)
      savehist-file (concat user-emacs-directory "tmp/savehist")
      set-mark-command-repeat-pop t
      transient-mark-mode nil
      use-package-always-defer t
      use-package-always-ensure t
      use-package-check-before-init t)


;; Never make tabs (exception for Makefiles)
(setq-default indent-tabs-mode nil)

;; Load the file containing variable customization
(load custom-file)

;; globalvars.el ends here
