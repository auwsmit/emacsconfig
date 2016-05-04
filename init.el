;; Disable garbage collection to speed start-up time
;; see: http://tiny.cc/7wd7ay
(let ((gc-cons-threshold most-positive-fixnum))

  ;; Setup load path
  (add-to-list 'load-path (expand-file-name "setup" user-emacs-directory))
  (require 'setup-package)

  ;; Setup appearance early so that if something goes
  ;; wrong with the init, Emacs is still lookin' good.
  (require 'setup-appearance)

  ;; Setup sensible defaults for Emacs
  ;; AKA general, non-specific settings
  (require 'setup-defaults)

  ;; Custom key bindings + which-key
  ;; (Most custom bindings are under their own setup)
  (require 'setup-keybindings)

  ;; Setup various packages
  (require 'setup-ido)
  (require 'setup-dired)
  (require 'setup-helm)

  ;; Setup Evil mode, along with associated packages
  ;; (This file is big and separated into functions)
  ;; (I recommend HideShow for easier code overview)
  (require 'setup-evil))
