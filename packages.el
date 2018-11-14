
;; Automatic indentation mode
(use-package aggressive-indent
  :ensure t
  :config (global-aggressive-indent-mode t)
  (setq aggressive-indent-excluded-modes
        (cons 'cider-repl-mode aggressive-indent-excluded-modes)))


(use-package all-the-icons
  :ensure t
  :config
  (when emacs4art-first-run-p
    (all-the-icons-install-fonts t)))

;; Every 4 days, try updateing the packages
(use-package auto-package-update
  :disabled
  ;; :ensure t
  :config
  (setq auto-package-update-delete-old-versions t
        auto-package-update-interval 4
        ;; apu--last-update-day-path
        ;; (expand-file-name
        ;;  apu-last-update-day-filename
        ;;  (concat user-emacs-directory "tmp"))
        )
  ;;(auto-package-update-maybe)
  )


(use-package autopair
  :ensure t
  :init (dolist
            (mode '(c-mode
                    json-mode js-mode
                    csound-mode c++-mode
                    typescript-mode web-mode))
          (add-hook (intern (concat (symbol-name mode) "-hook"))
                    (lambda () (autopair-mode 1)))))


;; The swiss-army knife for Clojure development
(use-package cider
  :ensure t
  :config (setq cider-repl-history-file
                (concat user-emacs-directory "tmp/cider-history")
                ;; Change nil to t enable the welcome message in Cider
                cider-repl-display-help-banner nil
                cider-repl-wrap-history t
                cider-repl-pop-to-buffer-on-connect 'display-only
                cider-show-error-buffer 'only-in-repl
                cider-eldoc-display-for-symbol-at-point nil
                ;; nrepl-prompt-to-kill-server-buffer-on-quit nil
                )
  ;; :init (unbind-key "C-c C-b" 'cider-mode-map)
  :bind (("C-c M-b" . cider-eval-buffer)
         ("C-c d"   . cider-print-docstring)))


(use-package cider-eval-sexp-fu
  :ensure t)

;; Basic clojure-mode with indent rules etc.
(use-package clojure-mode
  :ensure t
  :init (eldoc-mode t)
  (paredit-mode t)
  :config
  (setq clojure-align-forms-automatically t))

(use-package clojure-mode-extra-font-locking
  :ensure t)

;; (use-package clj-refactor
;;   :ensure t
;;   :config (add-hook 'clojure-mode-hook #'clj-refactor-mode))


;; Powerful auto-completions
(use-package company
  :ensure t
  :config
  (global-company-mode))

(use-package company-tern
  :ensure t
  :config
  (add-to-list 'company-backends 'company-tern))

(use-package cmake-mode
  :ensure t
  :mode (("CMakeLists.txt" . cmake-mode)
         ("\\.cmake\\'"    . cmake-mode)))

(use-package csound-mode
  :disabled
  :ensure t
  :mode (("\\.csd\\'" . csound-mode)
         ("\\.orc\\'" . csound-mode)
         ("\\.sco\\'" . csound-mode)
         ("\\.udo\\'" . csound-mode)))

(use-package css-mode
  :ensure t)

;; :box '(:line-width -1 :color "#4c83ff")
;; Let's use the default Emacs-Live theme
(use-package cyberpunk-theme
  :ensure t
  :config
  (when (display-graphic-p)
    (load-theme 'cyberpunk t)))


;; Dim Emacs when it's out of focus
(use-package dimmer
  :ensure t
  :init (dimmer-mode)
  :config (setq dimmer-fraction 0.35))

(use-package eshell-git-prompt
  :ensure t :defer t
  :init
  (eshell-git-prompt-use-theme 'powerline))

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

(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize)))

(use-package faust-mode
  :ensure t
  :config (setq faust-enable-smie nil)
  :mode (("\\.dsp$" . faust-mode))
  :init (setq-local indent-line-function #'js2-indent-line))

(use-package flycheck-clojure
  :ensure t
  :config
  (setq flycheck-highlighting-mode 'columns)
  (eval-after-load 'flycheck '(flycheck-clojure-setup))
  ;; (add-hook 'after-init-hook #'global-flycheck-mode)
  ;; (add-hook 'cider-mode-hook
  ;;           (lambda ()
  ;;             (setq next-error-function #'flycheck-next-error-function)))
  )

;; (use-package flycheck-pos-tip
;;   :ensure t
;;   :config
;;   (eval-after-load 'flycheck
;;     '(setq flycheck-display-errors-function #'flycheck-pos-tip-error-messages)))


;; (use-package flycheck-status-emoji
;;   :ensure t
;;   :init
;;   (unless (member "Symbola" (font-family-list))
;;     (download-symbola-font))
;;   (set-fontset-font "fontset-default" nil
;;                     (font-spec :size 20 :name "Symbola"))
;;   (flycheck-status-emoji-mode t))

(use-package flycheck-color-mode-line
  :ensure t
  :config
  (eval-after-load "flycheck"
    '(add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode)))

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
  :init
  (unbind-key " " ido-common-completion-map)
  (unbind-key " " ido-completion-map)
  :config
  (setq ido-enable-prefix nil
        ido-enable-flex-matching nil
        ido-use-filename-at-point nil
        ido-auto-merge-work-directories-length -1
        ido-use-virtual-buffers t
        ido-save-directory-list-file
        (expand-file-name "ido.hist" (concat user-emacs-directory "tmp/")))
  ;; Block annoying ido popup on space
  (defun ido-complete-space () (interactive))
  (custom-set-faces
   ;; Face used by ido for highlighting subdirs in the alternatives.
   '(ido-subdir ((t (:foreground "#7b68ee"))))
   ;; Face used by ido for highlighting first match.
   '(ido-first-match ((t (:foreground "#ff69b4"))))
   ;; Face used by ido for highlighting only match.
   '(ido-only-match ((t (:foreground "#ffcc33"))))
   ;; Face used by ido for highlighting its indicators (don't actually use this)
   '(ido-indicator ((t (:foreground "#ffffff"))))
   ;; Ido face for indicating incomplete regexps. (don't use this either)
   '(ido-incomplete-regexp ((t (:foreground "#ffffff")))))
  (ido-mode t)
  (ido-everywhere 1))

(use-package ido-vertical-mode
  :ensure t
  :config
  (ido-vertical-mode 1)
  (setq ido-vertical-define-keys 'C-n-C-p-up-down-left-right
        ido-vertical-show-count t
        ido-use-faces t)
  (set-face-attribute 'ido-vertical-first-match-face nil
                      :background "#262626")
  (set-face-attribute 'ido-vertical-only-match-face nil
                      :background "#e52b50"
                      :foreground "black"
                      )
  (set-face-attribute 'ido-vertical-match-face nil
                      :foreground "#000000"
                      ))

(use-package json-mode
  :ensure t
  :mode (("\\.json\\'" . json-mode))
  :config (add-hook 'json-mode-hook
                    (lambda ()
                      (make-local-variable 'js-indent-level)
                      (setq js-indent-level 2))))


;; powerful git management
;; docs: https://magit.vc/manual/magit/
(use-package magit
  :ensure t
  :config (global-magit-file-mode)
  :bind (("C-x g"   . magit-status)
         ("C-x M-g" . magit-dispatch-popup)))

;; Semi-graphical file explorer
(use-package neotree
  :ensure t
  :config
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow)
        neo-mode-line-type 'none
        neo-smart-open t
        neo-show-hidden-files t
        neo-cwd-line-style 'none
        neo-click-changes-root t)

  (defun neontree-updir ()
    (interactive)
    (prog2
        (save-excursion
          (neo-global--open-dir
           (neo-path--updir
            (neo-path--get-working-dir))))
        (next-line 2)))

  (add-hook 'neo-enter-hook
            (lambda (type & r) (if (equal type 'file)
                                   (neotree-hide))))

  (global-unset-key (kbd "C-x f"))
  (global-set-key (kbd "C-x f") #'neotree)
  (define-key neotree-mode-map
    (kbd "<backspace>")
    'neontree-updir))

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
  (add-hook 'cider-repl-mode-hook #'paredit-mode)
  (add-hook 'clojure-mode-hook #'paredit-mode))

;; Highlight matching parenthesis
(use-package paren
  :ensure t
  :config
  (show-paren-mode +1))

(use-package projectile
  :ensure t
  :config
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode +1))

(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'eval-expression-minibuffer-setup-hook #'rainbow-delimiters-mode)
  (add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'cider-repl-mode #'rainbow-delimiters-mode))

(use-package sesman
  :ensure t)

(use-package shell-pop
  :ensure t
  :defer t
  :init
  (custom-set-variables
   ;; custom-set-variables was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   '(shell-pop-shell-type (quote ("eshell" "*eshell*" (lambda nil (eshell)))))
   '(shell-pop-term-shell "/bin/bash")
   '(shell-pop-universal-key "C-'")
   '(shell-pop-window-size 30)
   '(shell-pop-full-span t)
   '(shell-pop-window-position "bottom"))
  (add-hook 'org-mode-hook (lambda ()
                             (define-key org-mode-map (kbd "C-'") 'shell-pop))))

(use-package smex
  :ensure t
  :bind ("M-x" . smex)
  :config
  (setq smex-save-file (concat user-emacs-directory "tmp/.smex-items")
        smex-history-length 40))

(use-package spaceline
  :ensure t
  :config
  (set-face-attribute
   'powerline-active2 nil
   :background "#000000" :foreground "#ffffff"))

(use-package spaceline-all-the-icons
  :ensure t
  :after spaceline
  :config (spaceline-all-the-icons-theme)
  (setq spaceline-all-the-icons-icon-set-modified 'toggle)
  (require 'spaceline-config)
  (spaceline-emacs-theme))


(use-package tabbar-ruler
  :defer t
  :ensure t
  :bind (([C-tab] . tabbar-forward-tab)
         ([C-S-iso-lefttab] . tabbar-backward-tab)
         ([C-f4] . kill-current-buffer)
         ("C-x <right>" . tabbar-forward)
         ("C-x <left>" . tabbar-backward))
  :init
  (progn
    (setq tabbar-ruler-global-tabbar t)
    (setq tabbar-ruler-fancy-close-image t)
    (require 'tabbar-ruler)
    (global-unset-key (kbd "C-c <C-up>"))
    (global-unset-key (kbd "C-c <C-down>")))
  :config
  (progn
    (setq left-margin-width 0)
    (set-fringe-mode 0)
    (setq right-margin-width 0)
    (tabbar-ruler-group-by-projectile-project)
    (custom-set-faces
     '(tabbar-button ((t (list
                          :inherit default
                          :box nil
                          :height 104
                          :width normal
                          :family "Sans Serif"
                          :foreground "#ffffff"))))
     '(tabbar-highlight ((t nil)))
     '(tabbar-selected ((t (list
                            :inherit default
                            :stipple nil
                            :weight normal
                            :height 150
                            :width normal
                            :family "Sans Serif"))))
     '(tabbar-selected-modified ((t (:inherit tabbar-default :background "#272822" :foreground "tomato" :box nil :height 150 :family "Sans Serif"))))
     '(tabbar-unselected ((t (:inherit tabbar-selected :background "#444" :foreground "#aaa" :height 160))))
     '(tabbar-unselected-modified ((t (:inherit tabbar-selected-modified :background "#444")))))))

(use-package typescript-mode
  :ensure t
  :mode (("\\.ts$" . web-mode)
         ("\\.tsx$" . web-mode)))

(use-package tide
  :ensure t
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         (before-save . tide-format-before-save))
  :mode (("\\.ts$" . web-mode)
         ("\\.tsx$" . web-mode)))

(use-package undo-tree
  :ensure t
  :config  (global-undo-tree-mode))

(use-package web-mode
  :ensure t
  :config (electric-indent-mode -1)
  (setq-default web-mode-comment-formats
                (remove '("javascript" . "/*") web-mode-comment-formats))
  (add-to-list 'web-mode-comment-formats '("javascript" . "//"))
  (add-to-list 'web-mode-comment-formats '("typescript" . "//"))
  (add-to-list 'web-mode-comment-formats '("jsx" . "//"))
  :mode (("\\.ts$" . web-mode)
         ("\\.tsx$" . web-mode)))

;; Auto complete shortcuts
(use-package which-key
  :ensure t
  :defer t
  :init
  (which-key-mode 1))

(use-package yasnippet
  :ensure t
  :config (yas-global-mode t)
  (add-hook 'clojure-mode-hook 'yas-minor-mode))

(use-package yaml-mode
  :ensure t)

(use-package yaml-tomato
  :ensure t)
