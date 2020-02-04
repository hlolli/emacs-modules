(use-package add-node-modules-path
  :ensure t)

;; Automatic indentation mode
(use-package aggressive-indent
  :ensure t
  :config (global-aggressive-indent-mode t)
  (setq aggressive-indent-excluded-modes
        (append aggressive-indent-excluded-modes
                (list 'cider-repl-mode 'java-mode 'js-mode 'web-mode))))

(use-package all-the-icons
  :ensure t
  :config
  (when emacs-modules-first-run-p
    (all-the-icons-install-fonts t)))

(use-package autodisass-java-bytecode
  :ensure t
  :defer t)

;; Every 4 days, try updating the packages
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
         ("C-c d"   . cider-print-docstring))
  :init (paredit-mode t))


(use-package cider-eval-sexp-fu
  :ensure t)

;; Basic clojure-mode with indent rules etc.
(use-package clojure-mode
  :ensure t
  :init
  (paredit-mode t)
  (electric-indent-mode t)
  (eldoc-mode t)
  :config
  (setq clojure-align-forms-automatically nil))

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

(use-package company-box
  :ensure t
  :hook (company-mode . company-box-mode))

(use-package company-flx
  :ensure t
  :after company
  :config
  (company-flx-mode +1)
  )

(use-package cmake-mode
  :ensure t
  :mode (("CMakeLists.txt" . cmake-mode)
         ("\\.cmake\\'"    . cmake-mode)))

(use-package css-mode
  :ensure t)

;; Let's use the default Emacs-Live theme
(use-package cyberpunk-theme
  :ensure t
  :config
  (when (display-graphic-p)
    (load-theme 'cyberpunk t)
    (custom-set-faces
     '(ido-subdir ((t (:foreground "#999999" :background "#0A0A0A"))))
     '(company-tooltip ((t (:background "#0C1021"  :foreground "#F8F8F8"))))
     '(js2-function-call ((t (:foreground "#6a5acd"))))
     '(js2-warning ((t (:foreground "#ff69b4"))))
     '(js2-warning-face ((t (:foreground "#ff69b4"))))
     '(js2-error ((t (:foreground "#ff69b4")))
                 ;; ((t (:foreground "#8b0000")))
                 )
     '(js2-error-face ((t (:foreground "#ff69b4")))
                      ;; ((t (:foreground "#8b0000")))
                      )
     '(js2-external-variable-face ((t (:foreground "#ffff00"))))
     '(js2-external-variable ((t (:foreground "#ffff00"))))
     )))


;; Dim Emacs when it's out of focus
;; (use-package dimmer
;;   :ensure t
;;   :init (when (display-graphic-p) (dimmer-mode))
;;   :config
;;   (setq dimmer-fraction 0.35)
;;   (setq dimmer-exclusion-regexp
;;         "^\*helm.*\\|^ \*Minibuf-.*\\|^ \*Echo.*\\|^ts-.*\\|.*\\.tsx?$\\|.*\\.jsx?$"))

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
  :mode (("\\.dsp$" . faust-mode)))

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

(use-package gradle-mode
  :ensure t)

(use-package google-c-style
  :defer t
  :ensure t
  :commands
  (google-set-c-style))

(use-package highlight-symbol
  :ensure t)

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

(use-package interaction-log
  :ensure t
  :init
  (global-set-key
   (kbd "C-h C-l")
   (lambda ()
     (interactive)
     (require 'interaction-log)
     (when (not ilog-recent-commands)
       (interaction-log-mode +1))
     (display-buffer ilog-buffer-name))))

(add-to-list 'auto-mode-alist `("\\.ts$" . js-mode))
(add-to-list 'auto-mode-alist `("\\.tsx$" . js-mode))

(use-package js2-mode
  :ensure t
  :config (setq js2-mode-show-parse-errors nil
                js2-mode-show-strict-warnings nil
                js2-strict-missing-semi-warning nil
                js2-missing-semi-one-line-override nil
                js2-mode-show-parse-errors nil
                js2-strict-inconsistent-return-warning nil
                js2-strict-cond-assign-warning nil
                js2-strict-var-redeclaration-warning nil
                js2-strict-var-hides-function-arg-warning nil
                js2-highlight-external-variables nil
                js2-include-jslint-globals nil
                js2-include-jslint-declaration-externs nil
                ;; js-mode
                js-jsx-syntax t
                )
  ;; :bind  (("M-." . 'js2-jump-to-defenition))
  :init
  ;; (define-key js-mode-map
  ;;   (kbd "M-.")
  ;;   'js2-jump-to-defenition)
  (add-hook 'js-mode-hook
            (lambda ()
              (js2-minor-mode)
              (hlolli/set-javascript-indent-from-prettier)
              (prettier-js-mode)
              (smartparens-mode t)
              (rainbow-delimiters-mode t)
              (rainbow-mode t)
              (highlight-symbol-mode t)
              (lsp))))

(use-package json-mode
  :ensure t
  :mode (("\\.json\\'" . json-mode))
  :init (add-hook 'json-mode-hook
                  (lambda ()
                    (make-local-variable 'js-indent-level)
                    (setq js-indent-level 2))))

;; LSP
(use-package lsp-ui :ensure t :commands lsp-ui-mode)
(use-package company-lsp :ensure t :commands company-lsp)
(use-package helm-lsp :ensure t :commands helm-lsp-workspace-symbol)
(use-package lsp-treemacs :ensure t :commands lsp-treemacs-errors-list)
(use-package lsp-mode
  :ensure t
  :commands lsp
  :config (setq lsp-eldoc-render-all t
                lsp-enable-completion-at-point t
                lsp-enable-indentation t
                lsp-enable-semantic-highlighting t
                lsp-enable-xref t
                lsp-enable-text-document-color t
                lsp-prefer-flymake :none
                lsp-ui-doc-enable nil)
  (add-to-list 'lsp-language-id-configuration
               '(".*.tsx?" . "typescript"))
  :init

  ;; (add-hook 'lsp-after-initialize-hook)
  ;; (add-hook 'js-mode-hook
  ;;           (lambda ()
  ;;             (hlolli/set-javascript-indent-from-prettier)
  ;;             (prettier-js-mode)
  ;;             (smartparens-mode t)
  ;;             (rainbow-delimiters-mode t)
  ;;             (rainbow-mode t)
  ;;             (highlight-symbol-mode t)
  ;; (js-jsx-mode)
  ;; (web-mode)

  ;; (require 'web-mode)
  ;; (setq web-mode-content-type "jsx"
  ;;       language "jsx"
  ;;       font-lock-support-mode nil
  ;;       font-lock-defaults
  ;;       '(web-mode-font-lock-keywords t)
  ;;       font-lock-extend-region-functions
  ;;       '(web-mode-extend-region)
  ;;       font-lock-unfontify-region-function
  ;;       'font-lock-default-unfontify-region
  ;;       font-lock-fontify-region-function
  ;;       (lambda (reg-beg reg-end &optional depth)
  ;;         (web-mode-part-highlight beg end)
  ;;         (web-mode-highlight-tags reg-beg reg-end)
  ;;         (web-mode-tag-highlight reg-beg reg-end)
  ;;         (web-mode-process-parts beg end 'web-mode-part-highlight)))
  ;; ))
  )

;; powerful git management
;; docs: https://magit.vc/manual/magit/
(use-package magit
  :ensure t
  :config (global-magit-file-mode)
  :hook magit-find-file
  :bind (("C-x g"   . magit-status)
	 ("C-x M-g" . magit-dispatch-popup)))

(use-package meghanada
  :defer t
  :init
  (add-hook 'java-mode-hook
            (lambda ()
              (google-set-c-style)
              (google-make-newline-indent)
              (meghanada-mode t)
              (smartparens-mode t)
              (rainbow-delimiters-mode t)
              (highlight-symbol-mode t)
              (add-hook 'before-save-hook 'meghanada-code-beautify-before-save)))

  :config
  (use-package realgud
    :ensure t)
  (setq indent-tabs-mode nil)
  (setq tab-width 2)
  (setq c-basic-offset 2)
  (setq meghanada-server-remote-debug t)
  (setq meghanada-javac-xlint "-Xlint:all,-processing")
  :bind
  (:map meghanada-mode-map
        ("C-S-t" . meghanada-switch-testcase)
        ("M-RET" . meghanada-local-variable)
        ("C-M-." . helm-imenu)
        ("M-r" . meghanada-reference)
        ("M-t" . meghanada-typeinfo)
        ("C-z" . hydra-meghanada/body))
  :commands
  (meghanada-mode))

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

(use-package nix-mode
  :ensure t
  :mode "\\.nix\\'")

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

(defvar-local prettier-location
  (concat emacs-modules-location "/third-party"))

(use-package prettier-js
  :ensure nil
  :load-path prettier-location
  :config
  (eval-after-load 'js-mode
    (lambda ()
      (if (or (locate-dominating-file default-directory ".prettierrc")
              (locate-dominating-file default-directory ".prettierrc.json"))
          (progn
            (add-hook 'js-mode-hook #'add-node-modules-path)
            (add-hook 'js-mode-hook #'hlolli/prettier-mode--disabled-on-ssh))))))

(use-package projectile
  :ensure t
  :config
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode +1))

(use-package rainbow-mode
  :ensure t)

(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'eval-expression-minibuffer-setup-hook #'rainbow-delimiters-mode)
  (add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'cider-repl-mode #'rainbow-delimiters-mode))

(use-package sesman
  :ensure t)

(use-package smartparens
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

(use-package shut-up
  :ensure t)

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
   :background "#3f3f3f" :foreground "#3E3D31"))

(use-package spaceline-all-the-icons
  :ensure t
  :after spaceline
  :config
  ;; (spaceline-all-the-icons-theme)
  (setq spaceline-all-the-icons-icon-set-modified 'toggle)
  (require 'spaceline-config)
  (spaceline-spacemacs-theme)
  ;; (spaceline-all-the-icons-theme)
  )

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
    (require 'tabbar-ruler)
    (setq tabbar-ruler-global-tabbar t)
    (setq tabbar-ruler-fancy-close-image t)
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

;; (use-package tern
;;   :ensure t
;;   :defer t
;;   :config
;;   (eval-after-load 'web-mode
;;     (lambda ()
;;       (tern-mode-enable))))

(use-package typescript-mode
  :ensure t
  :hook (typescript-mode . (lambda ()
                             (lsp)
                             (electric-indent-local-mode t)
                             (smartparens-mode 1)
                             (rainbow-delimiters-mode t)
                             (rainbow-mode t)
                             (highlight-symbol-mode t)
                             (prettier-js-mode)))
  ;; :mode (("\\.ts$" . typescript-mode)
  ;;        ("\\.tsx$" . typescript-mode))
  )

;; (defun setup-tide-mode ()
;;   "Set up Tide mode."
;;   (interactive)
;;   (tide-setup)
;;   (flycheck-mode +1)
;;   (setq flycheck-check-syntax-automatically '(save-mode-enabled))
;;   (eldoc-mode +1)
;;   (tide-hl-identifier-mode +1)
;;   (company-mode +1))
;; (use-package tide
;;   :ensure t
;;   :config
;;   (setq company-tooltip-align-annotations t)
;;   (add-hook 'typescript-mode-hook #'setup-tide-mode))

(use-package undo-tree
  :ensure t
  :defer t
  :config  (global-undo-tree-mode))

(use-package web-mode
  :ensure t
  :defer t
  :init
  ;; (add-hook 'js-mode-hook 'web-mode)
  ;; (add-hook 'js-mode-hook 'hlolli/set-javascript-indent-from-prettier)
  (add-hook 'web-mode-hook 'hlolli/set-javascript-indent-from-prettier)
  ;; (hlolli/replace-alist-mode auto-mode-alist 'js-mode 'web-mode)
  ;; (hlolli/replace-alist-mode auto-mode-alist 'mhtml-mode 'web-mode)
  (add-hook 'web-mode-hook
            (lambda ()
              (let ((ext (or (file-name-extension (if (stringp buffer-file-name) buffer-file-name "")))))
                (electric-indent-local-mode t)
                (paredit-everywhere-mode 1)
                (rainbow-delimiters-mode t)
                (rainbow-mode t)
                (highlight-symbol-mode t)
                (prettier-js-mode)
                ;; (when (or (string-equal "jsx" ext)
                ;;           (string-equal "js" ext)
                ;;           (string-equal "tsx" ext)
                ;;           (string-equal "ts" ext))
                ;;   (lsp)
                ;;   (setq web-mode-enable-auto-opening nil
                ;;         web-mode-enable-auto-indentation nil
                ;;         web-mode-enable-control-block-indentation nil
                ;;         web-mode-auto-close-style nil
                ;;         web-mode-auto-quote-style nil)
                ;;   (add-to-list 'web-mode-indentation-params '("lineup-args" . nil))
                ;;   (add-to-list 'web-mode-indentation-params '("lineup-calls" . nil))
                ;;   (add-to-list 'web-mode-indentation-params '("lineup-concats" . nil))
                ;;   (add-to-list 'web-mode-indentation-params '("lineup-quotes" . nil))
                ;;   (add-to-list 'web-mode-indentation-params '("lineup-ternary" . nil))
                ;;   (add-to-list 'web-mode-indentation-params '("case-extra-offset" . nil))
                ;;   (add-to-list 'web-mode-indentation-params '("lineup-ternary" . nil))
                ;;   (add-hook 'before-save-hook
                ;;             (lambda ()
                ;;               (web-mode-highlight-region (point-min) (point-max)))
                ;;             nil 'local)
                ;;   ;; (tern-mode t)
                ;;   ;; (setq-local web-mode-content-type "jsx")
                ;;   ;; (add-to-list 'company-backends 'company-tern)
                ;;   )
                ;; (when (or (string-equal "tsx" ext)
                ;;           (string-equal "ts" ext))
                ;;   (setup-tide-mode))
                )))
  :config  (setq-default web-mode-comment-formats
                         (remove '("javascript" . "/*") web-mode-comment-formats)
                         web-mode-enable-auto-quoting nil)
  (add-to-list 'web-mode-comment-formats '("javascript" . "//"))
  (add-to-list 'web-mode-comment-formats '("typescript" . "//"))
  (add-to-list 'web-mode-comment-formats '("jsx" . "//"))
  (setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'")))
  :mode (
         ;; ("\\.ts$" . web-mode)
         ;; ("\\.js$" . web-mode)
         ;; ("\\.jsx$" . web-mode)
         ("\\.css$" . web-mode)
         ("\\.scss$" . web-mode)
         ("\\.json$" . web-mode)
         ("\\.html$" . web-mode)
         ("\\.php$" . web-mode)
         ;; ("\\.tsx$" . web-mode)
         ))

;; Auto complete shortcuts
(use-package which-key
  :ensure t
  :defer t
  :init
  (which-key-mode 1))

(use-package xclip
  :ensure t
  :defer t
  :init (xclip-mode 1))

(use-package yasnippet
  :ensure t
  :config (yas-global-mode t)
  (add-hook 'clojure-mode-hook 'yas-minor-mode))

(use-package yaml-mode
  :ensure t)

(use-package yaml-tomato
  :ensure t)
