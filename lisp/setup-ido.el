;; Interactively do things by default
(ido-mode t)
(setq ido-create-new-buffer 'always)


;; More Evil friendly bindings
(define-key ido-common-completion-map "\C-j" 'ido-next-match)
(define-key ido-common-completion-map "\C-k" 'ido-prev-match)

(provide 'setup-ido)
