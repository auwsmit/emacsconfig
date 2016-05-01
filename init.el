;; Setup load path
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'setup-package)

;; Setup appearance early so that if something goes
;; wrong with the init, Emacs is still lookin' good.
(require 'setup-appearance)

;; General settings
(require 'setup-general)

;; Custom key bindings
(require 'setup-keybindings)

;; Setup various packages
(require 'setup-evil)
(require 'setup-ido)
(require 'setup-dired)
(require 'setup-helm)
