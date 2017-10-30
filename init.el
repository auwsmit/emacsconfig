;; org-babel-load-file increases startup time, so only do it if necessary.
;; To reload any config changes, delete config.el and restart emacs.


;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(if (file-exists-p "~/.emacs.d/config.el")
    (load-file "~/.emacs.d/config.el")
    ;; else
    (org-babel-load-file "~/.emacs.d/config.org"))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (ivy-hydra counsel lispy sentence-navigation nlinum-relative evil-magit evil-visualstar evil-nerd-commenter evil-exchange evil-surround projectile ssh-agency magit ace-window dired+ worf org-bullets multi-term s restart-emacs which-key hydra general dtrt-indent rainbow-delimiters smart-mode-line use-package)))
 '(show-paren-delay 0.0))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(nlinum-relative-current-face ((t (:weight normal))))
 '(trailing-whitespace ((t (:background "dim gray")))))
