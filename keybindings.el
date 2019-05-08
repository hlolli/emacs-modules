
;; Comment or uncomment region
(global-set-key (kbd "C-;") 'comment-or-uncomment-region-or-line)

;; Comment or uncomment region - terminal mode
(global-unset-key (kbd "C-x ;"))
(global-set-key (kbd "C-x ;") 'comment-or-uncomment-region-or-line)

;; Resize buffer window
(global-set-key (kbd "C-c <left>") 'shrink-window-horizontally--double)
(global-set-key (kbd "C-c <right>") 'enlarge-window-horizontally--double)
(global-set-key (kbd "C-c <up>") 'shrink-window--double)
(global-set-key (kbd "C-c <down>") 'enlarge-window--double)

;; replace the selected region via yank
(global-set-key (kbd "C-x y") 'yank-replace)


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
(global-set-key (kbd "C-x <up>") 'increment-number-decimal)
(global-set-key (kbd "C-x <down>") 'decrement-number-decimal)
(global-set-key (kbd "C-x <C-up>") 'increment-number-decimal)
(global-set-key (kbd "C-x <C-down>") 'decrement-number-decimal)

;; Disable annoying ctrl-z freeze
(global-unset-key (kbd "C-z"))

;; Use tabbar binding for these keybindings
(global-unset-key (kbd "C-x <right>"))
(global-unset-key (kbd "C-x <left>"))
