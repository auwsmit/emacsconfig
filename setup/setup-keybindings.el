;; Use general.el to make binding keys easier
(use-package general)

;; ibuffer instead of the simple default buffer list
(general-define-key "C-x C-b" 'ibuffer)

;; resize text easier
(general-define-key
 "C-0" (lambda() (interactive) (text-scale-set 0))
 "C-=" 'text-scale-increase
 "C--" 'text-scale-decrease)

;; Show available key bindings
(if (version< emacs-version "24.4")
    ;; compatible with older versions of Emacs
    (use-package guide-key
      :init
      (setq guide-key/guide-key-sequence t
            guide-key/recursive-key-sequence-flag t
            guide-key/popup-window-position 'bottom
            guide-key/idle-delay 0.5)
      :config
      (guide-key-mode 1))
  ;; only compatible with 24.4 and higher
  (use-package which-key
    :init
    (setq which-key-idle-delay 0.5
          which-key-side-window-max-height 0.50))
    :config
    (which-key-mode)
    ;; show top-level bindings
    (general-define-key "C-h T" 'which-key-show-top-level))

(provide 'setup-keybindings)
