;; Splash screen is not needed
(setq inhibit-splash-screen t)

;; Scroll smoothly
(setq scroll-margin 0
      scroll-conservatively 10000
      scroll-preserve-screen-position)

;; Remember last position when reopening files
(if (version< emacs-version "25.0") (progn
				      (require 'saveplace)
				      (setq-default save-place t))
  (save-place-mode 1))

;; Backup files in one folder
(setq backup-directory-alist `(("." . "~/.emacs.d/.saves")))

;; Allow recursive minibuffers
(setq enable-recursive-minibuffers t)

;; Don't be so stingy on the memory, we have lots now. It's the distant future.
(setq gc-cons-threshold 20000000)

;; Enable HideShow
(add-hook 'prog-mode-hook (lambda () (hs-minor-mode t)))

;; Same-name buffer distinction
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(provide 'setup-general)
