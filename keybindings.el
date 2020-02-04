;; Disable ctrl-m, use ctrl-j instead
(global-unset-key (kbd "C-t"))

(global-unset-key (kbd "C-x ."))

;; Disable annoying suspense-frame
(global-unset-key (kbd "C-x C-z"))

;; Comment or uncomment region
(global-set-key (kbd "C-;") 'hlolli/comment-or-uncomment-region-or-line)

;; Comment or uncomment region - terminal mode
(global-unset-key (kbd "C-x ;"))
(global-set-key (kbd "C-x ;") 'hlolli/comment-or-uncomment-region-or-line)

;; Resize buffer window
(global-set-key (kbd "C-c <left>") 'hlolli/shrink-window-horizontally--double)
(global-set-key (kbd "C-c <right>") 'hlolli/enlarge-window-horizontally--double)
(global-set-key (kbd "C-c <up>") 'hlolli/shrink-window--double)
(global-set-key (kbd "C-c <down>") 'hlolli/enlarge-window--double)

;; replace the selected region via yank
(global-set-key (kbd "C-x y") 'hlolli/yank-replace)

;; quickly move to the beginning or end of buffer
(global-set-key (kbd "C->") #'beginning-of-buffer)
(global-set-key (kbd "C-<") #'end-of-buffer)
(global-set-key (kbd "C-x C->") #'beginning-of-buffer)
(global-set-key (kbd "C-x C-<") #'end-of-buffer)


;; quickly move to beginning or end in terminal mode
(global-unset-key (kbd "C-x <"))
(global-unset-key (kbd "C-x >"))
(global-set-key (kbd "C-x >") #'beginning-of-buffer)
(global-set-key (kbd "C-x <") #'end-of-buffer)

;; Vim style increment and decrement
;; (global-unset-key (kbd "C-x <down>"))
(global-set-key (kbd "C-x <up>") 'hlolli/increment-number-decimal)
(global-set-key (kbd "C-x <down>") 'hlolli/decrement-number-decimal)
(global-set-key (kbd "C-x <C-up>") 'hlolli/increment-number-decimal)
(global-set-key (kbd "C-x <C-down>") 'hlolli/decrement-number-decimal)

;; Disable annoying ctrl-z freeze
(global-unset-key (kbd "C-z"))

;; Use tabbar binding for these keybindings
(global-unset-key (kbd "C-x <right>"))
(global-unset-key (kbd "C-x <left>"))

;; Git grep feature introduced in emacs27
(global-set-key (kbd "C-x p") 'vc-git-grep-template)

;; Macos meta key fix
(when (eq system-type 'darwin)
  (setq mac-option-key-is-meta nil
        mac-command-key-is-meta t
        mac-command-modifier 'meta
        mac-option-modifier 'none))
