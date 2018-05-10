(defgroup emacs4art nil
  "Maintain a menu of recently opened files."
  :version "1.0.0")

(defcustom emacs4art-location (concat user-emacs-directory "emacs4art")
  "Directory location of emacs 4 artists, defaults to ~/.emacs.d/emacs4art"
  :group 'emacs4art
  :type 'string)

(defcustom emacs4art-font "Fira Mono"
  "The global font, if not found, 
   then defaults to `DejaVu Sans Mono'"
  :group 'emacs4art
  :type 'string)

(require 'package)

;; Some global vars that need to be defined very early
(setq package--init-file-ensured t
      recentf-save-file (expand-file-name "tmp/recentf" user-emacs-directory))

(package-initialize)

(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)

;; First run of this emacs config
(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package)
  ;; create tmp dir
  (unless (file-exists-p (concat user-emacs-directory "tmp"))
    (make-directory (concat user-emacs-directory "tmp"))
    (make-directory (concat user-emacs-directory "tmp/autosaves"))
    (make-directory (concat user-emacs-directory "tmp/auto-save-list"))))

(eval-when-compile
  (require 'use-package))

;; Load packages
(load-file (expand-file-name "packages.el" emacs4art-location))

;; Load functions
(load-file (expand-file-name "functions.el" emacs4art-location))

;; Load keybindings
(load-file (expand-file-name "keybindings.el" emacs4art-location))

;; Load global config
(load-file (expand-file-name "global.el" emacs4art-location))

;; Load other extras
(load-file (expand-file-name "other.el" emacs4art-location))

;; initialize.el ends here
