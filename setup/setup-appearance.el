;; Maximize Emacs frame on startup
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Minimal GUI, remove unnecessary gui elements
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Splash screen is not needed
(setq inhibit-splash-screen t)

;; STOP THE BEEPING AND FLASHING
(setq ring-bell-function 'ignore)

;; Default Font
(add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono-9" ))
(set-face-attribute 'default t :font "DejaVu Sans Mono-9")

;; Theme
(use-package monokai-theme
  :config
  (load-theme 'monokai t))

(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;; Highlight matching parenthesis
(custom-set-variables '(show-paren-delay 0.0))
(show-paren-mode t)

;; Highlight trailing white space
(custom-set-variables '(show-trailing-whitespace t))
(custom-set-faces '(trailing-whitespace ((t (:background "dim gray")))))

;; use tildes to indicate lines after EOF
(define-fringe-bitmap 'tilde [0 0 0 113 219 142 0 0] nil nil 'center)
(setcdr (assq 'empty-line fringe-indicator-alist) 'tilde)
(set-fringe-bitmap-face 'tilde 'font-lock-comment-face)
(setq-default indicate-empty-lines t)

;; Show column number in modeline
(column-number-mode t)

;; Pretty modeline
(use-package smart-mode-line
  :config
  (setq sml/no-confirm-load-theme t)
  (setq sml/theme 'dark)
  (sml/setup))

;; Unclutter modeline
(use-package diminish)
(eval-after-load "hideshow" '(diminish 'hs-minor-mode))
(eval-after-load "undo-tree" '(diminish 'undo-tree-mode))
(eval-after-load "which-key" '(diminish 'which-key-mode))
(eval-after-load "simple" '(diminish 'overwrite-mode))
;; (eval-after-load "smartparens" '(diminish 'smartparens-mode))

(provide 'setup-appearance)
