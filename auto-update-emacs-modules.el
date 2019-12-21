;; WARNING: Anything modified in this file will be automatically removed!
;; Every time emacs-modules autoupdates, it will replace all the files with
;; the newest version from github. If you wish to supress this behaviour
;; please add (setq hlolli/emacs-modules-autoupdate-p nil) to your init.el


(defcustom hlolli/emacs-modules-autoupdate-p t
  "If true, emacs-modules will be updated each time
  `auto-package-update' updates (defaults to every
   4 days). If you plan to modify any of the files
   inside ~/.emacs.d/emacs-modules, you'll need to set
   this option to `nil' by inserting the following
   statement inside your ~/.emacs.d/init.el

   (setq hlolli/emacs-modules-autoupdate-p nil)
   "
  :group 'hlolli
  :type 'boolean)
