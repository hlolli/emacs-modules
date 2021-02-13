;;; fast-project-find-file.el --- TODO: description  -*- coding: utf-8 -*-

;; Copyright (C) 2020 Murilo Pereira

;; Author: Murilo Pereira <murilo@murilopereira.com>
;; Maintainer: Murilo Pereira <murilo@murilopereira.com>
;; URL: https://github.com/mpereira/fast-project-find-file
;; Package-Requires: ((emacs "25.2") (ivy) (dash))
;; Keywords: maint
;; SPDX-License-Identifier: GPL-3+

;; This file is not part of GNU Emacs.

;;; Commentary:

;; TODO.

;; TODO: Communicate when the rg process finishes somehow. Show something in the
;; ivy prompt?

;; TODO: Make it work with file-local lexical-binding.

;; TODO: Make it work without ivy.

;; TODO: Make it work without dash.

;; TODO: Make it work with `find`.

;; TODO: Make ivy filtering styles work.

;; TODO: Add unit tests.

;; TODO: Make it possible to clear caches.

;; TODO: Make it work with other VC systems than git.

;;; Code:

(defvar ivy--all-candidates)
(defvar ivy-regex)

(declare-function -filter "ext:dash.el" (pred list))
(declare-function ivy--format "ext:ivy.el" (cands))
(declare-function ivy--insert-minibuffer "ext:ivy.el" (text))
(declare-function ivy--set-candidates "ext:ivy.el" (x))
(declare-function ivy-re-to-str "ext:ivy.el" (re))

(defvar fast-project-find-file-debug-mode nil
  "TODO: docstring.")

(defvar fast-project-find-file-executable-find-cache (make-hash-table :test 'equal)
  "TODO: docstring.")

(defvar fast-project-find-file-project-root-cache (make-hash-table :test 'equal)
  "TODO: docstring.")

(defun fast-project-find-file-get-cached-or-compute (cache key-fn compute-fn)
  "TODO: CACHE KEY-FN COMPUTE-FN docstring."
  (let* ((key (funcall key-fn))
         (cached-value (gethash key cache)))
    (if cached-value
        (progn
          (when fast-project-find-file-debug-mode
            (message "cache hit: %s -> %s" key cached-value))
          cached-value)
      (let ((value (funcall compute-fn)))
        (when fast-project-find-file-debug-mode
          (message "cache miss: %s" key))
        (puthash key value cache)))))

(defun fast-project-find-file-executable-find (executable)
  "TODO: EXECUTABLE docstring."
  (interactive)
  (fast-project-find-file-get-cached-or-compute
   fast-project-find-file-executable-find-cache
   (lambda () (let ((host (file-remote-p default-directory 'host)))
                (list host executable)))
   (lambda () (executable-find executable t))))

(defun fast-project-find-file-project-root (&optional file-name)
  "TODO: FILE-NAME docstring."
  (interactive)
  (let ((file-name* (or (and (boundp 'file-name) file-name)
                        default-directory)))
    (fast-project-find-file-get-cached-or-compute
     fast-project-find-file-project-root-cache
     (lambda () file-name*)
     (lambda () (locate-dominating-file file-name* ".git")))))

(defun fast-project-find-file (&optional initial-input)
  "TODO: INITIAL-INPUT docstring."
  (interactive)
  (if-let ((fd-executable (fast-project-find-file-executable-find "fd")))
      (let* ((default-directory (fast-project-find-file-project-root))
             (make-process-fn (if (file-remote-p default-directory)
                                  'tramp-handle-make-process
                                'make-process))
             (filter-fn (lambda (candidate)
                          (string-match-p (ivy-re-to-str ivy-regex) candidate)))
             (process-name "fast-project-find-file")
             (candidates))
        (funcall
         make-process-fn
         :name process-name
         :filter (lambda (_process-buffer output-batch)
                   (when (boundp 'candidates)
                     (let ((candidate-batch (split-string output-batch "\n" t)))
                       (setq candidates (append candidates candidate-batch))
                       (ivy--set-candidates (-filter filter-fn candidates))
                       (ivy--insert-minibuffer (ivy--format ivy--all-candidates)))))
         :command (list "fd" "." "--color=never")
         :sentinel #'ignore
         :connection-type 'pipe)
        (ivy-read "File: "
                  (lambda (_input)
                    (if candidates
                        (-filter filter-fn candidates)
                      '("Searching...")))
                  :initial-input initial-input
                  :dynamic-collection t
                  :unwind (lambda ()
                            (when-let ((process (get-process process-name)))
                              (delete-process process)))
                  :action (lambda (candidate)
                            (find-file candidate))
                  :caller 'fast-project-find-file-project-find-file))
    (message "Coudn't find `fd` in PATH")))

(provide 'fast-project-find-file)

;;; fast-project-find-file.el ends here
