;; Minimal GUI, remove unnecessary gui elements
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; STOP THE BEEPING AND FLASHING
(setq ring-bell-function 'ignore)

;; Highlight matching parenthesis
(show-paren-mode t)

;; Show column number in modeline
(column-number-mode t)

;; Default Font
(add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono-9" ))
(set-face-attribute 'default t :font "DejaVu Sans Mono-9")

;; Maximize Emacs frame on startup
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; use tildes to indicate lines after EOF
(define-fringe-bitmap 'tilde [0 0 0 113 219 142 0 0] nil nil 'center)
(setcdr (assq 'empty-line fringe-indicator-alist) 'tilde)
(set-fringe-bitmap-face 'tilde 'font-lock-comment-face)
(setq-default indicate-empty-lines t)

;; TODO: personally modify monokai
(use-package monokai-theme
  :config
  (load-theme 'monokai t))

;; RainbowDelimiters
(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;; SmartModeLine
(use-package smart-mode-line
  :config
  (setq sml/no-confirm-load-theme t)
  (setq sml/theme 'dark)
  (sml/setup))

;; memes
(use-package nyan-mode
  :config
  (setq nyan-bar-length 8)
  ;; (nyan-toggle-wavy-trail)
  ;; (nyan-start-animation)
  (nyan-mode t)
  )

;; Unclutter modeline
(use-package diminish)
;;(eval-after-load "smartparens" '(diminish 'smartparens-mode))
(eval-after-load "HideShow" '(diminish 'hs-minor-mode))
(eval-after-load "undo-tree" '(diminish 'undo-tree-mode))

;; Pretty powerline
;; (use-package powerline-evil
;;   :config
;;   (powerline-evil-vim-color-theme)
;;   (custom-set-faces
;;    '(powerline-evil-normal-face ((t (:background "#859900" :foreground "#eee8d5"))))
;;    '(powerline-evil-insert-face ((t (:background "#268bd2" :foreground "#eee8d5"))))
;;    '(powerline-evil-visual-face ((t (:background "#cb4b16" :foreground "#eee8d5"))))
;;    '(powerline-evil-operator-face ((t (:background "#2aa198" :foreground "#eee8d5"))))
;;    '(powerline-evil-replace-face ((t (:background "#dc322f" :foreground "#eee8d5"))))
;;    '(powerline-evil-motion-face ((t (:background "#d33682" :foreground "#eee8d5"))))
;;    '(powerline-evil-emacs-face ((t (:background "#6c71c4" :foreground "#eee8d5"))))
;;    ))

(provide 'setup-appearance)
