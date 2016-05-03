;; ibuffer instead of the simple default buffer list
(global-set-key (kbd "C-x C-b") 'ibuffer)

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
