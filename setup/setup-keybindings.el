;; Use general.el to make binding keys easier
(use-package general)

;; ibuffer instead of the simple default buffer list
(general-define-key "C-x C-b" 'ibuffer)

;; resize text easier
(general-define-key "C-0"
  (lambda() (interactive) (text-scale-set 0)))
(general-define-key "C-+" 'text-scale-increase)
(general-define-key "C-=" 'text-scale-increase)
(general-define-key "C-_" 'text-scale-decrease)
(general-define-key "C--" 'text-scale-decrease)

;; TODO: Check minimum version
;; which-key to help my stupid monkey brain
(use-package which-key
  :init
  (setq which-key-idle-delay 0.5)
  :config
  (which-key-mode)
  (general-define-key "C-h W" 'which-key-show-top-level))

;; ;; guide-key for the newbie
;; (use-package guide-key
;;   :init
;;   (setq guide-key/guide-key-sequence
;; 	'("SPC" "C-x" "C-c" "M-g")
;; 	guide-key/recursive-key-sequence-flag t
;; 	guide-key/popup-window-position 'bottom
;; 	guide-key/idle-delay 0.5)
;;   :config
;;   (guide-key-mode 1))

(provide 'setup-keybindings)
