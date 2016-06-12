;; `org-babel-load-file` increases startup time, so only do it if necessary.
;; To reload any config changes, delete config.el and restart emacs.
(if (file-exists-p "~/.emacs.d/config.el")
    (load-file "~/.emacs.d/config.el")
    (org-babel-load-file "~/.emacs.d/config.org"))
