;; WARNING: Anything modified in this file will be automatically removed!
;; Every time emacs4art autoupdates, it will replace all the files with
;; the newest version from github. If you wish to supress this behaviour
;; please add (setq emacs4art-autoupdate-p nil) to your init.el


(defcustom emacs4art-autoupdate-p t
  "If true, emacs4art will be updated each time
  `auto-package-update' updates (defaults to every
   4 days). If you plan to modify any of the files
   inside ~/.emacs.d/emacs4art, you'll need to set
   this option to `nil' by inserting the following
   statement inside your ~/.emacs.d/init.el
   
   (setq emacs4art-autoupdate-p nil)
   "
  :group 'emacs4art
  :type 'boolean)


