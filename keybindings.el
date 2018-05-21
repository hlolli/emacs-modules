
;; Comment or uncomment region
(global-set-key (kbd "C-;") 'comment-or-uncomment-region-or-line)

;; Resize buffer window
(global-set-key (kbd "C-c <C-left>") 'shrink-window-horizontally--double)
(global-set-key (kbd "C-c <C-right>") 'enlarge-window-horizontally--double)
(global-set-key (kbd "C-c <C-up>") 'shrink-window--double)
(global-set-key (kbd "C-c <C-down>") 'enlarge-window--double)

;; replace the selected region via yank
(global-set-key (kbd "C-x y") 'yank-replace)


;; quickly move to the beginning or end of buffer
(global-set-key (kbd "C-<") #'beginning-of-buffer)
(global-set-key (kbd "C->") #'end-of-buffer)


;; Vim style increment and decrement
;; (global-unset-key (kbd "C-x <down>"))
(global-set-key (kbd "C-x <up>") 'increment-number-decimal)
(global-set-key (kbd "C-x <down>") 'decrement-number-decimal)
(global-set-key (kbd "C-x <C-up>") 'increment-number-decimal)
(global-set-key (kbd "C-x <C-down>") 'decrement-number-decimal)
