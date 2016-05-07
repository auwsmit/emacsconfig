(use-package helm
  :diminish helm-mode
  :init
  (require 'helm-config)
  (setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
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
   '(helm-M-x-fuzzy-match t)
   '(helm-apropos-fuzzy-match t)
   '(helm-completion-in-region-fuzzy-match t))

  ;; No arrow keys
  (general-define-key :keymaps 'helm-map
                      "C-j" 'helm-next-line
                      "C-k" 'helm-previous-line
                      "C-n" 'helm-next-source
                      "C-p" 'helm-previous-source)

  ;; ;; Helm for default M-x
  ;; (global-set-key (kbd "M-x") 'helm-M-x)
  ;; ;; old M-x
  ;; (global-set-key (kbd "C-x x") 'execute-extended-command)

  ;; ;; Possible fuzzy search improvements
  ;; (use-package helm-flx
  ;;   :config
  ;;   (helm-flx-mode +1))
  ;; (use-package helm-fuzzier
  ;;   :config
  ;;   (helm-fuzzier-mode 1))
  )

(provide 'setup-helm)
