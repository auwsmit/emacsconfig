(use-package helm
  :diminish helm-mode
  :init
  (require 'helm-config)
  (setq helm-split-window-in-side-p         t ; open helm buffer inside current window, not occupy whole other window
      helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
      helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-ff-file-name-history-use-recentf t)

  :config
  (helm-mode 1)
  ;; Fuzzy like CtrlP.. we'll see how this affects performance
  (custom-set-variables
   '(helm-recentf-fuzzy-match t)
   '(helm-buffers-fuzzy-matching t)
   ;; '(helm-locate-fuzzy-match t)
   '(helm-M-x-fuzzy-match t)
   ;; '(helm-semantic-fuzzy-match t)
   ;; '(helm-imenu-fuzzy-match t)
   ;; '(helm-apropos-fuzzy-match t)
   ;; '(helm-lisp-fuzzy-completion t)
   '(helm-completion-in-region-fuzzy-match t)
   )

  ;; More Evil friendly bindings
  (define-key helm-map (kbd "C-j") 'helm-next-line)
  (define-key helm-map (kbd "C-k") 'helm-previous-line)
  (define-key helm-map (kbd "C-n") 'helm-next-source)
  (define-key helm-map (kbd "C-p") 'helm-previous-source)

  ;; Helm execute-command
  (global-set-key (kbd "M-x") 'helm-M-x)

  ;; Possible fuzzy search improvements
  ;; (use-package helm-flx
  ;;   :config
  ;;   (helm-flx-mode +1))
  ;; (use-package helm-fuzzier
  ;;   :config
  ;;   (helm-fuzzier-mode 1))
  )

(provide 'setup-helm)
