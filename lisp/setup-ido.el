;; Interactively do things by default
(ido-mode 1)
(setq ido-create-new-buffer 'always)

;; Show results vertically
(use-package ido-vertical-mode
  :config
  (ido-vertical-mode 1))

;; Evil-friendly bindings
(define-key ido-common-completion-map "\C-j" 'ido-next-match)
(define-key ido-common-completion-map "\C-k" 'ido-prev-match)
(define-key ido-file-completion-map "\C-k" 'ido-prev-match)
(define-key ido-buffer-completion-map "\C-k" 'ido-prev-match)

(provide 'setup-ido)
