(defgroup hlolli nil
  "Maintain a menu of recently opened files."
  :version "1.0.0")

(defcustom emacs-modules-location (concat user-emacs-directory "emacs-modules")
  "Directory location of Emacs 4 artists, defaults to ~/.emacs.d/emacs-modules"
  :group 'hlolli
  :type 'string)

(defcustom emacs-modules-font "Fira Mono"
  "The global font, if not found,
   then defaults to `DejaVu Sans Mono'"
  :group 'hlolli
  :type 'string)

(defvar emacs-modules-first-run-p nil)

(require 'package)
(require 'cl)

;; Some global vars that need to be defined very early
(setq package--init-file-ensured t
      recentf-save-file (expand-file-name "tmp/recentf" user-emacs-directory))

(package-initialize)

;; (exec-path-from-shell-initialize)

(setq package-archives
      '(("gnu"     . "http://elpa.gnu.org/packages/")
	("melpa"   . "https://melpa.org/packages/"))
      package-archive-priorities
      '(("gnu"     . 0)))

;; First run of this emacs config
(when (not (package-installed-p 'use-package))
  (setq emacs-modules-first-run-p t)
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
(load-file (expand-file-name "packages.el" emacs-modules-location))

;; Load functions
(load-file (expand-file-name "functions.el" emacs-modules-location))

;; Load keybindings
(load-file (expand-file-name "keybindings.el" emacs-modules-location))

;; Load global config
(load-file (expand-file-name "global.el" emacs-modules-location))

;; Load other extras
(load-file (expand-file-name "other.el" emacs-modules-location))

;; Load third-party libraries
(load-file
 (expand-file-name
  "yarn.el"
  (concat (file-name-as-directory  emacs-modules-location)
          "third-party")))

;; initialize.el ends here
