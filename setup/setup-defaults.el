;; Make emacsclient work

(require 'server)
(if (not (server-running-p)) (server-start))

(setq
 ;; Backup files in one folder
 backup-directory-alist `(("." . "~/.emacs.d/.saves"))

 ;; Switch to help window when activated
 help-window-select t

 ;; Scroll smoothly
 scroll-margin 0
 scroll-conservatively 10000
 scroll-preserve-screen-position t

 ;; Sentences end with a single period.
 sentence-end-double-space nil

 ;; Allow recursive minibuffers
 enable-recursive-minibuffers t)

;; y/n instead of yes/no
(fset 'yes-or-no-p 'y-or-n-p)

;; Default tab/indent settings
(setq-default tab-width 4
              indent-tabs-mode nil
              c-basic-offset 4
              c-default-style "linux")
(c-set-offset 'case-label '+)

;; Auto detect indent settings
(use-package dtrt-indent)

;; Auto-update changed files
(global-auto-revert-mode t)

;; When saving a file that starts with `#!', make it executable in *nix
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

;; Enable HideShow in programming modes
(add-hook 'prog-mode-hook (lambda () (hs-minor-mode t)))

;; Better same-name buffer distinction
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; Remember last position for reopened files
(if (version< emacs-version "25.0")
    (progn
      (require 'saveplace)
      (setq-default save-place t))
  (save-place-mode 1))

;; Disable garbage collection in minibuffer
(defun my-minibuffer-setup-hook ()
  (setq gc-cons-threshold most-positive-fixnum))
(defun my-minibuffer-exit-hook ()
  (setq gc-cons-threshold 800000))
(add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)

(provide 'setup-defaults)
