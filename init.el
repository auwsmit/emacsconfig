;; org-babel-load-file increases startup time, so only do it if necessary.
;; To reload any config changes, delete config.el and restart emacs.


;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(if (file-exists-p "~/.emacs.d/config.el")
    (load-file "~/.emacs.d/config.el")
  (org-babel-load-file "~/.emacs.d/config.org"))
