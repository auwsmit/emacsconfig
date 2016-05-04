;; ibuffer instead of the simple default buffer list
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; resize text easier
(define-key global-map (kbd "C-0")
  (lambda() (interactive) (text-scale-set 0)))
(define-key global-map (kbd "C-+") 'text-scale-increase)
(define-key global-map (kbd "C-=") 'text-scale-increase)
(define-key global-map (kbd "C-_") 'text-scale-decrease)
(define-key global-map (kbd "C--") 'text-scale-decrease)

;; ;; TODO: figure out why this won't work??
;; (use-package which-key
;;   :config
;;   (which-key-mode))

;; guide-key for the newbie
(use-package guide-key
  :init
  (setq guide-key/guide-key-sequence
	'("SPC" "C-x" "C-c" "M-g")
	guide-key/recursive-key-sequence-flag t
	guide-key/popup-window-position 'bottom
	guide-key/idle-delay 0.5)
  :config
  (guide-key-mode 1))

(provide 'setup-keybindings)
