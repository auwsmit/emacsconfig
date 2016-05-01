(defun preconfig-evil ()
  "Configure Evil mode variables BEFORE activation."
  (setq evil-want-fine-undo "No")
  (setq evil-want-C-u-scroll t)
  (setq evil-ex-substitute-global t))

(defun config-evil-custom-bindings ()
  "Define custom key bindings for Evil mode."

  ;; C-g like Vim to see total line numbers
  (define-key evil-normal-state-map "\C-g" 'count-words)
  
  ;; C-g cancel consistency.. it's like the new escape
  (define-key evil-insert-state-map "\C-g" 'evil-normal-state)
  (define-key evil-visual-state-map "\C-g" 'evil-normal-state)
  (define-key evil-replace-state-map "\C-g" 'evil-normal-state)

  ;; U instead of C-r
  (define-key evil-normal-state-map "U" 'redo)

  ;; Q to replay q register
  (define-key evil-normal-state-map "Q" (kbd "@ q"))

  ;; Y to yank until EOL more like D and C
  (define-key evil-normal-state-map "Y" (kbd "Y $"))

  ;; Backspace instead of C-6 for alternate-file
  (define-key evil-normal-state-map (kbd "DEL") 'evil-switch-to-windows-last-buffer)

  ;; [S]plit Line (sister to [J]oin Line)
  (defun my/SplitLine ()
    (interactive)
    (newline-and-indent)
    (forward-line -1)
    (move-end-of-line 1))
  (define-key evil-normal-state-map "S" 'my/SplitLine)

  ;; Insert motion
  (define-key evil-insert-state-map "\C-a" 'move-beginning-of-line)
  (define-key evil-insert-state-map "\C-e" 'move-end-of-line)

  ;; select pasted text
  (define-key evil-normal-state-map (kbd "g p") (kbd "` [ v ` ]"))

  ;; unimpaired (work in progress)
  (defun my/evil-blank-above (count)
    "Add [count] blank lines above the point."
    (interactive "p")
    (while (> count 0)
      (evil-insert-newline-above)
      (forward-line 1)
      (add-hook 'post-command-hook #'evil-maybe-remove-spaces)
      (setq count (1- count))))
  (defun my/evil-blank-below (count)
    "Add [count] blank lines below the point."
    (interactive "p")
    (while (> count 0)
      (evil-insert-newline-below)
      (forward-line -1)
      (add-hook 'post-command-hook #'evil-maybe-remove-spaces)
      (setq count (1- count))))
  (define-key evil-normal-state-map (kbd "[ SPC") 'my/evil-blank-above)
  (define-key evil-normal-state-map (kbd "] SPC") 'my/evil-blank-below))

(defun config-evil-other-modes ()
  "Define custom key bindings for other modes to be more consistent
  with Evil mode."

  ;; ibuffer
  (evil-set-initial-state 'ibuffer-mode 'normal)
  (define-key evil-normal-state-map (kbd "g b") 'ibuffer)

  ;; shell
  (defun my/evil-shell-insert ()
    (interactive)
    (evil-goto-line)
    (evil-append-line 0))
  (evil-define-key 'normal shell-mode-map
    "I" 'my/evil-shell-insert
    "A" 'my/evil-shell-insert)

  ;; Dired
  ;; TODO: Get shell access vim-style
  ;; tpope vinegar-style
  (define-key evil-normal-state-map "-" (kbd "\C-x d RET"))
  (add-hook 'dired-mode-hook
	    (lambda ()
	      (define-key dired-mode-map (kbd "-") ;; go up directory
		(lambda () (interactive) (find-alternate-file "..")))
	      ))
  (evil-define-key 'normal dired-mode-map
    (kbd "RET") 'dired-find-alternate-file
    "q" 'my/evil-delete-buffer-keep-windows)

  ;; Info
  (evil-define-key 'normal Info-mode-map
    "w" 'evil-forward-word-begin
    "b" 'evil-backward-word-begin
    "e" 'evil-forward-word-end
    "?" 'evil-search-backward
    "n" 'evil-search-next
    "N" 'evil-search-previous
    "G" 'evil-goto-line
    "gg" 'evil-goto-first-line
    "]]" 'Info-forward-node
    "[[" 'Info-backward-node
    "]n" 'Info-next
    "[n" 'Info-prev
    "<" 'Info-top-node
    ">" 'Info-final-node))

(defun config-evil ()
  "Configure Evil mode."

  ;; Normal state == Motion state
  ;; This simplifies things for me.
  (setq evil-normal-state-modes (append evil-motion-state-modes evil-normal-state-modes))
  (setq evil-motion-state-modes nil)

  ;; Start these modes in Normal state
  (evil-set-initial-state 'debugger-mode 'normal)
  (evil-set-initial-state 'Custom-mode 'normal)

  ;; Use Evil search over Emacs search
  ;; (C-s is still i-search)
  (custom-set-variables
   '(evil-search-module (quote evil-search)))

  ;; Dehighlight Evil search on cursor move
  ;; TODO: Somehow make this only active during evil search.
  ;;       It's probably inefficient having this hook active all the time.
  (defun my/evil-search-nohighlight-on-move ()
    (interactive)
    (if (not (or (equal (this-command-keys) "n")
		 (equal (this-command-keys) "N")))
	(evil-ex-nohighlight)))
  (add-hook 'pre-command-hook 'my/evil-search-nohighlight-on-move)

  ;; Dehighlight on insert mode or visual mode
  (add-hook 'evil-insert-state-entry-hook 'evil-ex-nohighlight)
  (add-hook 'evil-visual-state-entry-hook 'evil-ex-nohighlight)

  (evil-define-command my/evil-delete-buffer-keep-windows
    (buffer &optional bang)
    (interactive "<b><!>")
    (with-current-buffer (or buffer (current-buffer))
      (when bang
	(set-buffer-modified-p nil)
	(dolist (process (process-list))
	  (when (eq (process-buffer process)
		    (current-buffer))
	    (set-process-query-on-exit-flag process nil))))
      (if (and (fboundp 'server-edit)
	       (boundp 'server-buffer-clients)
	       server-buffer-clients)
	  (server-edit)
	(kill-buffer nil))))

  (config-evil-custom-bindings)
  (config-evil-other-modes))

(defun config-evil-leader ()
  "Configure evil leader mode."
  (evil-leader/set-leader "<SPC>")
  (setq evil-leader/in-all-states 1)

  (defun my/open-init-el ()
    (interactive)
    (find-file "~/.emacs.d/init.el"))

  (evil-leader/set-key
    "S"  'shell
    "i"  'my/open-init-el
    "w"  'evil-write
    "x"  'evil-delete-buffer
    "X"  'my/evil-delete-buffer-keep-windows
    "b"  'ido-switch-buffer
    "f"  'ido-find-file
    "hr"  'helm-recentf
    "hb"  'helm-buffers-list
    "hf"  'helm-find-files
    "<SPC>" 'other-window))

(use-package evil
  :init
  (preconfig-evil)
  :config
  (add-hook 'evil-mode-hook 'config-evil)

  (use-package evil-leader
    :config
    (config-evil-leader)
    (global-evil-leader-mode))

  (use-package evil-surround
    :config
    (global-evil-surround-mode))

  (use-package evil-exchange
    :config
    (evil-exchange-install))

  (use-package evil-visualstar
    :config
    (global-evil-visualstar-mode))

  ;; (use-package linum-relative
  ;;   :init
  ;;   ;; Show current line number
  ;;   (setq linum-relative-current-symbol "")
  ;;   :config
  ;;   ;; Relative line numbers for visual selections
  ;;   (add-hook 'evil-visual-state-entry-hook (lambda() (interactive) (linum-mode 1)))
  ;;   (add-hook 'evil-visual-state-exit-hook (lambda() (interactive) (linum-mode 0)))

  ;;   (linum-relative-on)
  ;;   (add-hook 'dired-mode-hook 'linum-on))

  (use-package evil-smartparens
    :diminish evil-smartparens-mode
    :config
    (defun enable-evil-smartparens ()
      ;; (turn-on-smartparens-strict-mode)
      (evil-smartparens-mode t)
      (evil-define-key 'normal evil-smartparens-mode-map
  	"S" 'my/SplitLine))
    (add-hook 'emacs-lisp-mode-hook 'enable-evil-smartparens))

  (evil-mode 1))

(provide 'setup-evil)
