;; Interactively do things by default
(ido-mode 1)
(setq ido-create-new-buffer 'always)

;; Show results vertically
(use-package ido-vertical-mode
  :config
  (ido-vertical-mode 1))

;; No arrow keys
(general-define-key
 :keymaps '(ido-common-completion-map
            ido-file-completion-map
            ido-buffer-completion-map)
 "C-j" 'ido-next-match
 "C-k" 'ido-prev-match)

(provide 'setup-ido)
