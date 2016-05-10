(defun setup-evil-settings ()
  "Primary function to configure Evil mode. All other 'setup-evil-'
functions are called from within this one."

  ;; Normal state == Motion state
  ;; This simplifies things greatly.
  (setq evil-normal-state-modes (append evil-motion-state-modes evil-normal-state-modes))
  (setq evil-motion-state-modes nil)

  ;; Cursor color/shape to indicate modes/states
  ;; (matches GVim cursor shapes)
  (setq evil-normal-state-cursor   '("dodger blue" box)
        evil-insert-state-cursor   '("dodger blue" bar)
        evil-replace-state-cursor  '("dodger blue" hbar)
        evil-operator-state-cursor '("dodger blue" (hbar . 7))
        evil-visual-state-cursor   '("orange" box)
        evil-motion-state-cursor   '("deep pink" box)
        evil-emacs-state-cursor    '("red2" box))

  ;; Use Evil search over Emacs search
  ;; (C-s/C-r are still i-search)
  (custom-set-variables
   '(evil-search-module (quote evil-search)))

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
  "Define custom key bindings for Evil mode. This is a
  sub-function of 'setup-evil-settings'"

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
  (general-define-key "C-j" 'evil-scroll-down
                      "C-k" 'evil-scroll-up)

  ;; Jump list (previous, next)
  (general-define-key "C-p" 'evil-jump-backward
                      "C-n" 'evil-jump-forward)

  ;; select last pasted text
  (general-define-key "g p" (kbd "` [ v ` ]"))

  ;; [g]o [s]ayonara (à la vim-sayonara)
  (general-define-key "g s" 'evil-delete-buffer
                      "g S" 'my/evil-delete-buffer-keep-windows)

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

  ;; =p/=P from unimpaired (super WIP)
  ;; TODO: figure out how to map to = p
  (defun my/evil-formatted-paste-below (count)
    "Paste after linewise, reindenting."
    (interactive "p")
    (end-of-line) (newline)
    (evil-paste-after)
    (evil-goto-mark 91)
    (evil-visual-line)
    (evil-goto-mark 93)
    ;; (evil-indent 91 93)
    ;; (evil-normal-state)
    )
  (general-define-key "] p" 'my/evil-formatted-paste-below)

  ;; "get option" is the mnemonic
  ;; also inspired by tpope's unimpaired
  (general-define-key "g o t" 'toggle-truncate-lines
                      "g o n" 'linum-mode
                      ;; TODO: look into cross-platform spell checker
                      "g o s" 'flyspell-mode)


  ;; Fold-to-scope
  (general-define-key "z s" 'hs-hide-level)

  ;; Just in case M-x is weirdly undefined
  (general-define-key "M-x" 'execute-extended-command)

  (general-define-key "SPC c" mode-specific-map)

  ;; C-g like Vim to see total line numbers
  (general-define-key "C-g" 'count-words)

  ;; general.el: back to global for non-normal-state bindings
  (setq general-default-keymaps 'global)

  ;; Always cancel/escape to normal state
  (general-define-key :states '(visual insert replace motion emacs)
                      "C-g" 'evil-normal-state
                      "C-[" 'evil-normal-state)

  ;; Insert navigation
  (general-define-key :states '(insert)
                      "C-a" 'move-beginning-of-line
                      "C-e" 'move-end-of-line)
  )
(defun setup-evil-other-modes ()
  "Define custom key bindings for other modes to be more consistent
  with Evil mode. This is a sub-function of
  'setup-evil-settings'"

  ;; ibuffer
  (evil-set-initial-state 'ibuffer-mode 'normal)

  ;; Occur
  (evil-set-initial-state 'occur-mode 'normal)
  (evil-make-overriding-map occur-mode-map 'normal)
  (general-evil-define-key 'normal
    :keymaps '(occur-mode-map occur-edit-mode-map)
    "q"   'evil-delete-buffer
    "RET" 'occur-mode-goto-occurrence
    "gg"  'evil-goto-first-line
    "n" 'evil-ex-search-next
    "?" 'evil-ex-search-backward)

  ;; Dired
  ;; bindings inspired by tpope's vinegar
  (general-define-key :states '(normal visual) "-" (kbd "C-x d RET"))
  (general-evil-define-key 'normal 'dired-mode-map
    ;; Go up directory
    "-" (lambda ()(interactive)
          (find-alternate-file ".."))
    "RET" 'dired-find-alternate-file
    "q" 'my/evil-delete-buffer-keep-windows
    "n" 'evil-ex-search-next
    "N" 'evil-ex-search-previous
    "?" 'evil-ex-search-backward)

  ;; Info
  (general-evil-define-key 'normal 'Info-mode-map
    "TAB" 'Info-next-reference
    "S-TAB" 'Info-prev-reference
    "RET" 'Info-follow-nearest-node
    "u" 'Info-up
    "q" 'Info-exit
    "d" 'Info-directory
    "C-p" 'Info-history-back
    "C-n" 'Info-history-forward
    "C-l" 'Info-history
    "]" 'Info-forward-node
    "[" 'Info-backward-node
    "<" 'Info-top-node
    ">" 'Info-final-node
    "_" 'Info-prev
    "+" 'Info-next)

  ;; Shell(s)
  (defun my/evil-shell-insert ()
    "Go to the very end of the buffer and enter insert state."
    (interactive)
    (evil-goto-line)
    (evil-append-line 0))
  (general-evil-define-key 'normal
    :keymaps '(shell-mode-map eshell-mode-map term-mode-map)
    "I" 'my/evil-shell-insert
    "A" 'my/evil-shell-insert)

  ;; Help
  (general-evil-define-key 'normal 'help-mode-map
    "q" 'evil-delete-buffer))
(defun setup-evil-leader ()
  "Configure bindings to emulate Vim Leader functionality. This is a
sub-function of 'setup-evil-settings'"

  (defun my/open-init-el ()
    (interactive)
    (find-file "~/.emacs.d/init.el"))

  (defun my/simulate-key-press (key)
    "Pretend that KEY was pressed.
KEY must be given in `kbd' notation."
    `(lambda () (interactive)
       (setq prefix-arg current-prefix-arg)
       (setq unread-command-events (listify-key-sequence (read-kbd-macro,key)))))

  (general-define-key
   :states '(normal motion insert emacs)
   :prefix "SPC"
   :non-normal-prefix "C-SPC"

   ;; Less CTRL more SPC
   "w" 'evil-window-map
   "x" (my/simulate-key-press "C-x")
   "c" (my/simulate-key-press "C-c")
   "h" (my/simulate-key-press "C-h")

   ;; Often used shortcuts
   "TAB" 'other-window
   "s"  'evil-write
   "e"  'eval-last-sexp
   "f"  'ido-find-file
   "b"  'ido-switch-buffer
   "B"  'ibuffer
   "i"  'my/open-init-el
   "o"  'occur
   "O"  'multi-occur

   ;; Helm shortcuts
   "SPC" 'helm-M-x
   "C-r"  'helm-recentf
   "C-b"  'helm-buffers-list
   "C-f"  'helm-find-files
   "C-h"  'helm-apropos))
(use-package evil
  :init
  (setq evil-ex-substitute-global t
        evil-want-fine-undo "No"
        evil-overriding-maps nil)
  :config
  (add-hook 'evil-mode-hook 'setup-evil-settings)

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
