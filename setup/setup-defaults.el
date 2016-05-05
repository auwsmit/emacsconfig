; Make emacsclient work
(server-start)

;; Scroll smoothly
(setq scroll-margin 0
      scroll-conservatively 10000
      scroll-preserve-screen-position)

;; When saving a file that starts with `#!', make it executable.
(add-hook 'after-save-hook
	  'executable-make-buffer-file-executable-if-script-p)

;; Sentences end with a single period.
(setq sentence-end-double-space nil)

;; Backup files in one folder
(setq backup-directory-alist `(("." . "~/.emacs.d/.saves")))

;; Auto-update changed files
(global-auto-revert-mode t)

;; y/n instead of yes/no
(fset 'yes-or-no-p 'y-or-n-p)

;; Enable HideShow in programming modes
(add-hook 'prog-mode-hook (lambda () (hs-minor-mode t)))

;; Auto detect indent settings
(use-package dtrt-indent)

;; Better same-name buffer distinction
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; Remember last position for reopened files
(if (version< emacs-version "25.0")
    (progn
      (require 'saveplace)
      (setq-default save-place t))
  (save-place-mode 1))

;; Allow recursive minibuffers
(setq enable-recursive-minibuffers t)

;; Disable garbage collection in minibuffer
(defun my-minibuffer-setup-hook ()
  (setq gc-cons-threshold most-positive-fixnum))
(defun my-minibuffer-exit-hook ()
  (setq gc-cons-threshold 800000))
(add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)

(provide 'setup-defaults)
