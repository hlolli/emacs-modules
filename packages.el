
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
