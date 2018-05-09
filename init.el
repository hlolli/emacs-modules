(require 'package)
(package-initialize)

(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)

;; First run of this emacs config
(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package)
  ;; create tmp dir
  (unless (file-exists-p (concat user-emacs-directory "tmp"))
    (make-directory (concat user-emacs-directory "tmp"))))

(eval-when-compile
  (require 'use-package))

;; Automatic indentation mode
(use-package aggressive-indent
  :ensure t
  :config (global-aggressive-indent-mode t)
  (setq aggressive-indent-excluded-modes
	(cons 'cider-repl-mode aggressive-indent-excluded-modes)))

;; Every 4 days, try updateing the packages
(use-package auto-package-update
  :ensure t
  :config
  (setq auto-package-update-delete-old-versions t
        auto-package-update-interval 4
	apu--last-update-day-path
	(expand-file-name
	 apu--last-update-day-filename
	 (concat user-emacs-directory "tmp")))
  (auto-package-update-maybe))


;; The swiss-army knife for Clojure development
(use-package cider
  :ensure t
  :config (setq cider-repl-history-file
		(concat user-emacs-directory "tmp/cider-history")
                ;; Change nil to t enable the welcome message in Cider
                cider-repl-display-help-banner nil
                cider-repl-wrap-history t
		cider-repl-pop-to-buffer-on-connect 'display-only)
  :bind (:map cider-mode-map
              ("C-c C-b" . cider-eval-buffer)
              ("C-c d"   . cider-print-docstring)))

;; Basic clojure-mode with indent rules etc.
(use-package clojure-mode
  :ensure t
  :init
  (add-hook 'clojure-mode-hook
            (lambda ()
              (paredit-mode)
              (eldoc-mode t)
              (rainbow-delimiters-mode)
              (flycheck-mode 1))))


;; Powerful auto-completions
(use-package company
  :ensure t
  :config
  (global-company-mode))

;; Let's use the default Emacs-Live theme
(use-package cyberpunk-theme
  :ensure t
  :config (when (display-graphic-p)
	    (load-theme 'cyberpunk t)))

;; Dim Emacs when it's out of focus
(use-package dimmer
  :ensure t
  :init (dimmer-mode))


;; Flashing eval
(use-package eval-sexp-fu
  :ensure t
  :init
  (unless (fboundp 'multiple-value-bind)
    (defalias 'multiple-value-bind 'cl-multiple-value-bind))
  (add-hook 'emacs-lisp-mode-hook #'turn-on-eval-sexp-fu-flash-mode)
  (add-hook 'emacs-lisp-mode-hook #'turn-on-eval-sexp-fu-flash-mode)
  (add-hook 'clojure-mode-hook #'turn-on-eval-sexp-fu-flash-mode)
  (add-hook 'cider-repl-mode-hook #'turn-on-eval-sexp-fu-flash-mode))

;; (use-package emacs-lisp-mode)

;; Show pre-written text in autocompletions
(use-package flx-ido
  :ensure t
  :init
  (flx-ido-mode 1)
  (setq ido-enable-flex-matching t
        ido-use-faces t))


;; Completions when finding files
(use-package ido
  :ensure t
  :config
  (setq ido-enable-prefix nil
        ido-enable-flex-matching t
        ido-use-filename-at-point nil
        ido-auto-merge-work-directories-length -1
        ido-use-virtual-buffers t
        ido-save-directory-list-file
        (expand-file-name "ido.hist" (concat user-emacs-directory "tmp/")))
  (ido-mode t)
  (ido-everywhere 1))

;; Paredit mode for structural editing
;; makes sure the brackets always match
;; learn the commands http://pub.gajendra.net/src/paredit-refcard.pdf
(use-package paredit
  :ensure t
  :config
  (add-hook 'emacs-lisp-mode-hook #'paredit-mode)
  ;; enable in the *scratch* buffer
  (add-hook 'lisp-interaction-mode-hook #'paredit-mode)
  (add-hook 'ielm-mode-hook #'paredit-mode)
  (add-hook 'lisp-mode-hook #'paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'paredit-mode)
  (add-hook 'cider-repl-mode-hook #'paredit-mode))

;; Highlight matching parenthesis
(use-package paren
  :config
  (show-paren-mode +1))

(use-package rainbow-delimiters
  :ensure t)

(use-package smex
  :ensure t
  :bind ("M-x" . smex)
  :config
  (setq smex-save-file (concat user-emacs-directory "tmp/.smex-items")))


(use-package yasnippet
  :ensure t
  :config (yas-global-mode t))


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


;; Try to load Fira Mono font it exists
(let ((default-font (if (member "Fira Mono" (font-family-list))
			"Fira Mono" "DejaVu Sans Mono")))
  (set-face-attribute 'default nil
		      :font default-font
		      :height 120
		      :weight 'normal
		      :width 'normal)
  ;; Enlarge just a tiny bit
  (text-scale-adjust +1))


(setq ac-ignore-case nil
      auto-save-list-file-prefix (concat user-emacs-directory "tmp/auto-save-list/.saves-")
      backup-directory-alist '(("." . (concat user-emacs-directory "tmp/autosaves")))
      custom-file (concat user-emacs-directory "tmp/custom.el")
      create-lockfiles nil
      electric-indent-inhibit t
      frame-title-format '((:eval (if (buffer-file-name) (abbreviate-file-name (buffer-file-name)) "%b")))
      user-full-name "John Smith"
      user-mail-address "example@email.com"
      ido-save-directory-list-file (concat user-emacs-directory "tmp/ido.last")
      inhibit-startup-message t
      require-final-newline t
      ring-bell-function 'ignore
      savehist-additional-variables '(kill-ring search-ring regexp-search-ring)
      savehist-file (concat user-emacs-directory "tmp/savehist")
      scroll-margin 2 ;; nice scrolling
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

;; init.el ends here

