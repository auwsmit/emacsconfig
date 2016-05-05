(defun setup-evil ()
  "Configure Evil mode."

  ;; Normal state == Motion state
  ;; This simplifies things for me.
  (setq evil-normal-state-modes (append evil-motion-state-modes evil-normal-state-modes))
  (setq evil-motion-state-modes nil)

  ;; Use Evil search over Emacs search
  ;; (C-s is still i-search)
  (custom-set-variables
   '(evil-search-module (quote evil-search)))

  ;; Cursor color to indicate modes
    (setq evil-normal-state-cursor   '("#0a9dff" box)
          evil-insert-state-cursor   '("#0a9dff" bar)
          evil-replace-state-cursor  '("#0a9dff" hbar)
          evil-operator-state-cursor '("cyan" box)
          evil-visual-state-cursor   '("yellow" box)
          evil-motion-state-cursor   '("deep pink" box)
          evil-emacs-state-cursor    '("red2" box))

  ;; Center evil search & dehighlight when finished searching
  (defun my/evil-search-nohighlight-on-move ()
    "Dehighlight Evil ex search when
any keys other than n or N are pressed."
    (interactive)
    (if (not (or (equal (this-command-keys) "n")
		 (equal (this-command-keys) "N")))
	(progn (evil-ex-nohighlight)
	       (remove-hook 'pre-command-hook
			    'my/evil-search-nohighlight-on-move))))
  (defun my/add-hook-evil-search ()
    (add-hook 'pre-command-hook 'my/evil-search-nohighlight-on-move))
  (defadvice evil-ex-start-search (after advice-for-evil-ex-start-search activate)
    (progn (evil-scroll-line-to-center (line-number-at-pos))
	   (my/add-hook-evil-search)))
  (defadvice evil-ex-search (after advice-for-evil-ex-search activate)
    (progn (evil-scroll-line-to-center (line-number-at-pos))
	   (my/add-hook-evil-search)))

  ;; this is the (evil-delete-buffer) function,
  ;; but doesn't close any windows
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

  ;; clear trailing whitespace ex command
  (evil-ex-define-cmd "ctw" 'delete-trailing-whitespace)

  (setup-evil-custom-bindings)
  (setup-evil-other-modes)
  (setup-evil-leader))

(defun setup-evil-custom-bindings ()
  "Define custom key bindings for Evil mode."

  ;; general.el: bind to normal-state by default
  (setq general-default-keymaps 'evil-normal-state-map)

  ;; U instead of C-r for redo
  (general-define-key "U" 'redo)

  ;; Q to replay q register
  (general-define-key "Q" (kbd "@ q"))

  ;; Y to yank until EOL more like D and C
  (general-define-key "Y" (kbd "y $"))

  ;; Back(space) to last buffer
  (general-define-key (kbd "DEL") 'evil-switch-to-windows-last-buffer)

  ;; [S]plit Line (sister to [J]oin Line)
  (defun my/SplitLine ()
    (interactive)
    (newline-and-indent)
    (forward-line -1)
    (move-end-of-line 1))
  (general-define-key "S" 'my/SplitLine)

  ;; this lets me keep Emacs' C-u (universal-argument)
  (general-define-key "C-j" 'evil-scroll-down)
  (general-define-key "C-k" 'evil-scroll-up)

  ;; Jump list (previous, next)
  (general-define-key "C-p" 'evil-jump-backward)
  (general-define-key "C-n" 'evil-jump-forward)

  ;; select last pasted text
  (general-define-key "g p" (kbd "` [ v ` ]"))

  ;; [g]o [s]ayonara (à la vim-sayonara)
  (general-define-key "g s" 'evil-delete-buffer)
  (general-define-key "g S" 'my/evil-delete-buffer-keep-windows)

  ;; Insert blank space above/below cursor
  ;; inspired by tpope's unimpaired
  (defun my/evil-blank-above (count)
    "Add [count] blank lines above the point."
    (interactive "p")
    (setq col (current-column))
    (while (> count 0)
      (evil-insert-newline-above)
      (forward-line 1)
      (add-hook 'post-command-hook #'evil-maybe-remove-spaces)
      (setq count (- count 1)))
    (move-to-column col))
  (defun my/evil-blank-below (count)
    "Add [count] blank lines below the point."
    (interactive "p")
    (setq col (current-column))
    (while (> count 0)
      (evil-insert-newline-below)
      (forward-line -1)
      (add-hook 'post-command-hook #'evil-maybe-remove-spaces)
      (setq count (- count 1)))
    (move-to-column col))
  (general-define-key "[ SPC" 'my/evil-blank-above)
  (general-define-key "] SPC" 'my/evil-blank-below)

  ;; "get option" is the mnemonic
  ;; also inspired by tpope's unimpaired
  (general-define-key "g o t" 'toggle-truncate-lines)
  (general-define-key "g o n" 'linum-mode)
  ;; TODO: look into cross-platform spell checker
  ;; (define-key evil-normal-state-map (kbd "g o s") 'flyspell-mode)

  ;; C-g like Vim to see total line numbers
  (general-define-key "C-g" 'count-words)

  ;; general.el: back to global for non-normal-state bindings
  (setq general-default-keymaps 'global)

  ;; Always cancel to normal state
  (general-define-key :states '(visual insert replace motion emacs)
		      "C-g" 'evil-normal-state
		      "C-[" 'evil-normal-state)

  ;; Insert motion
  (general-define-key :states '(insert)
		      "C-a" 'move-beginning-of-line
		      "C-e" 'move-end-of-line)
)

(defun setup-evil-other-modes ()
  "Define custom key bindings for other modes to be more consistent
  with Evil mode."

  ;; ibuffer
  (evil-set-initial-state 'ibuffer-mode 'normal)
  (general-define-key :states 'normal "g b" 'ibuffer)

  ;; Shell
  (defun my/evil-shell-insert ()
    "Go to the very end of the buffer and enter insert state."
    (interactive)
    (evil-goto-line)
    (evil-append-line 0))
  (general-evil-define-key 'normal 'shell-mode-map
    "I" 'my/evil-shell-insert
    "A" 'my/evil-shell-insert)

  ;; Dired
  ;; inspired by tpope's vinegar
  (general-define-key :states '(normal visual) "-" (kbd "C-x d RET"))
  (general-evil-define-key '(normal visual) 'dired-mode-map
    ;; Go up directory
    "-" (lambda () (interactive) (find-alternate-file ".."))
    "RET" 'dired-find-alternate-file
    "q" 'my/evil-delete-buffer-keep-windows)

  ;; Info
  (general-evil-define-key 'normal 'Info-mode-map
    "w" 'evil-forward-word-begin
    "b" 'evil-backward-word-begin
    "e" 'evil-forward-word-end
    "f" 'evil-find-char
    "?" 'evil-ex-search-backward
    "n" 'evil-ex-search-next
    "N" 'evil-ex-search-previous
    "H" 'evil-window-top
    "L" 'evil-window-bottom
    "G" 'evil-goto-line
    "gg" 'evil-goto-first-line
    "]]" 'Info-forward-node
    "[[" 'Info-backward-node
    "<" 'Info-top-node
    ">" 'Info-final-node
    "C-p" 'Info-history-back
    "C-n" 'Info-history-forward))

(defun setup-evil-leader ()
  "Configure bindings to emulate Vim Leader functionality."

  (defun my/open-init-el ()
    (interactive)
    (find-file "~/.emacs.d/init.el"))

  (general-define-key
   :states '(normal replace motion emacs)
   :prefix "SPC"
   :global-prefix "C-SPC"
   "TAB" 'other-window
   "SPC" 'helm-M-x
   "e"  'eval-last-sexp
   "i"  'my/open-init-el
   "w"  'evil-write
   "b"  'ido-switch-buffer
   "f"  'ido-find-file
   "hr"  'helm-recentf
   "hb"  'helm-buffers-list
   "hf"  'helm-find-files
   "ha"  'helm-apropos))

(use-package evil
  :init
  (setq evil-ex-substitute-global t
	evil-want-fine-undo "No")
  :config
  (add-hook 'evil-mode-hook 'setup-evil)

  ;; Manipulate surroundings
  (use-package evil-surround
    :config
    (global-evil-surround-mode))

  ;; Exchange operator
  (use-package evil-exchange
    :config
    ;; "[g]o e[x]change"
    ;; gX is cancel
    (evil-exchange-install))

  ;; Comment operator
  (use-package evil-nerd-commenter
    :config
    (general-define-key :states '(normal visual) "g c" 'evilnc-comment-operator))

  ;; Search visual selections
  (use-package evil-visualstar
    :config
    (global-evil-visualstar-mode))

  ;; ;; Relative line numbers
  ;; ;; TODO: look into enabling only in active window
  ;; (use-package linum-relative
  ;;   :init
  ;;   ;; Show current line number
  ;;   (setq linum-relative-current-symbol "")
  ;;   :config
  ;;   ;; Enabled for visual selections
  ;;   (add-hook 'evil-visual-state-entry-hook (lambda() (interactive) (linum-mode 1)))
  ;;   (add-hook 'evil-visual-state-exit-hook (lambda() (interactive) (linum-mode 0)))
  ;;   ;; Enable for Dired
  ;;   (add-hook 'dired-mode-hook 'linum-on)
  ;;   (linum-relative-on))

  (evil-mode 1))

(provide 'setup-evil)
