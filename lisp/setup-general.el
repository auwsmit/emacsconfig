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

;; Disable garbage collection in minibuffer
(defun my-minibuffer-setup-hook ()
  (setq gc-cons-threshold most-positive-fixnum))
(defun my-minibuffer-exit-hook ()
  (setq gc-cons-threshold 800000))
(add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)

;; Enable HideShow
(add-hook 'prog-mode-hook (lambda () (hs-minor-mode t)))

;; Auto detect indent settings
(use-package dtrt-indent)

;; Better same-name buffer distinction
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(provide 'setup-general)
