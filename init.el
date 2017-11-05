;; org-babel-load-file increases startup time, so only do it if necessary.
;; To reload any config changes, delete config.el and restart emacs.

;;(package-initialize)

(if (file-exists-p "~/.emacs.d/config.el")
    (load-file "~/.emacs.d/config.el")
    ;; else
  (org-babel-load-file "~/.emacs.d/config.org"))
;;(custom-set-variables
;; ;; custom-set-variables was added by Custom.
;; ;; If you edit it by hand, you could mess it up, so be careful.
;; ;; Your init file should contain only one such instance.
;; ;; If there is more than one, they won't work right.
;; '(package-selected-packages
;;   (quote
;;    (worf which-key use-package ssh-agency smart-mode-line sentence-navigation s restart-emacs rainbow-delimiters projectile org-bullets nlinum-relative multi-term lispy ivy-hydra general evil-visualstar evil-surround evil-nerd-commenter evil-magit evil-exchange dtrt-indent dired+ counsel)))
;; '(show-paren-delay 0.0))
;;(custom-set-faces
;; ;; custom-set-faces was added by Custom.
;; ;; If you edit it by hand, you could mess it up, so be careful.
;; ;; Your init file should contain only one such instance.
;; ;; If there is more than one, they won't work right.
;; '(nlinum-relative-current-face ((t (:weight normal))))
;; '(trailing-whitespace ((t (:background "dim gray")))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (eclipse-theme worf which-key use-package ssh-agency smart-mode-line sentence-navigation s restart-emacs rainbow-delimiters projectile org-bullets nlinum-relative multi-term lispy ivy-hydra general evil-visualstar evil-surround evil-nerd-commenter evil-magit evil-exchange dtrt-indent dired+ counsel)))
 '(show-paren-delay 0.0))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(nlinum-relative-current-face ((t (:weight normal))))
 '(trailing-whitespace ((t (:background "dim gray")))))
