(require 'web-mode)

(defun web-mode-tag-highlight (&optional beg end)
  (unless beg (setq beg (point)))
  (unless end (setq end (1+ (web-mode-tag-end-position beg))))
  (let (name type face flags slash-beg slash-end bracket-end)
    (setq flags (get-text-property beg 'tag-beg)
          type (get-text-property beg 'tag-type)
          name (get-text-property beg 'tag-name))
    (setq bracket-end (> (logand flags 16) 0))
    (cond
     ((eq type 'comment)
      (put-text-property beg end 'font-lock-face 'web-mode-comment-face)
      (when (and web-mode-enable-comment-interpolation (> (- end beg) 5))
        (web-mode-interpolate-comment beg end nil)))
     ((eq type 'cdata)
      (put-text-property beg end 'font-lock-face 'web-mode-doctype-face))
     ((eq type 'doctype)
      (put-text-property beg end 'font-lock-face 'web-mode-doctype-face))
     ((eq type 'declaration)
      (put-text-property beg end 'font-lock-face 'web-mode-doctype-face))
     (name
      (setq slash-beg (> (logand flags 4) 0)
            slash-end (> (logand flags 8) 0)
            bracket-end (> (logand flags 16) 0))
      (setq face (cond
                  ((not bracket-end)       'web-mode-html-tag-unclosed-face)
                  ((and web-mode-enable-element-tag-fontification
                        (setq face (cdr (assoc name web-mode-element-tag-faces))))
                   face)
                  ((> (logand flags 32) 0) 'web-mode-html-tag-namespaced-face)
                  ((> (logand flags 2) 0)  'web-mode-html-tag-custom-face)
                  (t                       'web-mode-html-tag-face)))
      (put-text-property beg (+ beg (if slash-beg 2 1))
                         'font-lock-face 'web-mode-html-tag-bracket-face)
      (unless (string= name "_fragment_")
        (put-text-property (+ beg (if slash-beg 2 1))
                           (+ beg (if slash-beg 2 1) (length name))
                           'font-lock-face face))
      (when (or slash-end bracket-end)
        (put-text-property (- end (if slash-end 2 1)) end 'font-lock-face 'web-mode-html-tag-bracket-face)
        ) ;when
      (when (> (logand flags 1) 0)
        (web-mode-highlight-attrs beg end))
      )
     )
    ))

(defun web-mode-highlight-tags (reg-beg reg-end &optional depth)
  (let ((continue t))
    (goto-char reg-beg)
    (when (and (not (get-text-property (point) 'tag-beg))
               (not (web-mode-tag-next)))
      (setq continue nil))
    (when (and continue (>= (point) reg-end))
      (setq continue nil))
    (while continue
      (cond
       (depth
        (when (eq depth (get-text-property (point) 'jsx-depth))
          (web-mode-tag-highlight))
        )
       (t
        (web-mode-tag-highlight))
       ) ;cond
      (when (or (not (web-mode-tag-next))
                (>= (point) reg-end))
        (setq continue nil))
      ) ;while
    (when web-mode-enable-inlays
      (when (null web-mode-inlay-regexp)
        (setq web-mode-inlay-regexp (regexp-opt '("\\[" "\\(" "\\begin{align}"))))
      (let (beg end expr)
        (goto-char reg-beg)
        (while (web-mode-dom-rsf web-mode-inlay-regexp reg-end)
          (setq beg (match-beginning 0)
                end nil
                expr (substring (match-string-no-properties 0) 0 2))
          (setq expr (cond
                      ((string= expr "\\[") "\\]")
                      ((string= expr "\\(") "\\)")
                      (t "\\end{align}")))
          (when (and (web-mode-dom-sf expr reg-end)
                     (setq end (match-end 0))
                     (not (text-property-any beg end 'tag-end t)))
            (font-lock-append-text-property beg end 'font-lock-face 'web-mode-inlay-face)
            ) ;when
          ) ;while
        ) ;let
      ) ;when
    (let (beg end)
      (goto-char reg-beg)
      (while (web-mode-dom-rsf "&\\([#]?[[:alnum:]]\\{2,8\\}\\);" reg-end)
        (setq beg (match-beginning 0)
              end (match-end 0))
        (when (not (text-property-any beg end 'tag-end t))
          (font-lock-append-text-property beg end 'font-lock-face 'web-mode-html-entity-face)
          ) ;when
        ) ;while
      ) ;let
    ))
