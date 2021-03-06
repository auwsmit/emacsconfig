;;; general.el --- Convenience wrappers for keybindings. -*- lexical-binding: t -*-

;; Author: Fox Kiester <noct@openmailbox.org>
;; URL: https://github.com/noctuid/general.el
;; Package-Version: 20171106.818
;; Created: February 17, 2016
;; Keywords: vim, evil, leader, keybindings, keys
;; Package-Requires: ((emacs "24.4") (cl-lib "0.5"))
;; Version: 0.1

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; This package provides convenient wrappers for more succinctly defining
;; keybindings. It allows defining multiple keys at once, specifying an
;; arbitrary number of named prefix keys to be used in key definitions,
;; implicitly wrapping key strings with (kbd ...), and more. It provides a
;; single function for standard emacs key definitions as well as evil key
;; definitions for any evil state and any keymap. It also provides a setup
;; function to generate "nmap", "vmap", etc. keybinding functions for evil.

;; For more information see the README in the online repository.

;;; Code:
(require 'cl-lib)

;; * Settings
(defgroup general nil
  "Gives convenient wrappers for key definitions."
  :group 'convenience
  :prefix 'general-)

(defcustom general-implicit-kbd t
  "Whether to implicitly wrap a (kbd) around keybindings.
This applies to the prefix key as well."
  :group 'general
  :type 'boolean)

(defcustom general-default-prefix nil
  "The default prefix key sequence to use."
  :group 'general
  :type 'string)

(defcustom general-default-non-normal-prefix nil
  "The default prefix key sequence to use for the 'emacs and 'insert states.
Note that this setting is only useful for evil-users and will only have an
effect when binding keys in the 'emacs and/or 'insert states or in the
'evil-insert-state-map and/or 'evil-emacs-state-map keymaps. When this is not
specified, `general-default-prefix' will be the default prefix for any states
and keymaps. If this is specified `general-default-prefix' or the arg to :prefix
will not be used when binding keys in the insert and emacs states."
  :group 'general
  :type 'string)

(defcustom general-default-global-prefix nil
  "The default prefix key sequence to use for all evil states.
This setting is only useful for evil users. Note that like with
`general-default-non-normal-prefix', if this or :global-prefix is specified,
`general-default-prefix' or the arg to :prefix will not be used for binding
keys in the insert and emacs states. If you don't need a different or extra
prefix for one or both state types (insert and emacs vs. the other states),
just use `general-default-prefix'/:prefix by itself."
  :group 'general
  :type 'string)

(define-widget 'general-state 'lazy
  "General's evil state type."
  :type '(choice
          (const :tag "Insert state" insert)
          (const :tag "Emacs state" emacs)
          (const :tag "Normal state" normal)
          (const :tag "Visual state" visual)
          (const :tag "Motion state" motion)
          (const :tag "Operator state" operator)
          (const :tag "Replace state" replace)
          (const :tag "Use define-key not evil-define-key" nil)
          ;; other packages define states
          symbol))

(defcustom general-default-states nil
  "The default evil state(s) to make mappings in.
Non-evil users should keep this nil."
  :group 'general
  :type '(choice general-state
                 (set general-state)))

(defcustom general-non-normal-states '(insert emacs hybrid iedit-insert)
  "List of \"non-normal\" evil states (used with :non-normal-prefix). When
  :states is not specified (only :keymaps), these will automatically be expanded
  to their full global evil keymap equivalents."
  :group 'general
  :type '(repeat general-state))

(define-widget 'general-keymap 'lazy
  "General's keymap type."
  :type '(choice
          (const :tag "Global keymap" global)
          (const :tag "Buffer local keymap" local)
          symbol))

(defcustom general-default-keymaps 'global
  "The default keymap(s) to bind keys in."
  :group 'general
  :type '(choice general-keymap
                 (repeat general-keymap)))

(defcustom general-vim-definer-default nil
  "Whether set the states or keymaps in a `general-create-vim-definer' function.
If nil, use the default from when the function was created. If 'keymaps, set the
default keymaps. If 'states, set the default states."
  :group 'general
  :type '(choice
          (const :tag "Default to setting :keymaps" keymaps)
          (const :tag "Default to setting :states" states)
          (const :tag "Use the initial default" nil)))

(defvar general-keybindings nil
  "Holds all the keybindings created with `general-define-key' (and wrappers).
This is an alist of a keymap to an alist of a state to keybindings.")

(defvar general-local-keybindings nil
  "Holds all the local keybindings created with `general-define-key'.
This is an alist of a state to keybindings.")
(make-variable-buffer-local 'general-local-keybindings)

(define-widget 'general-alist 'lazy
  "General's alist type."
  :type '(alist :key-type (or symbol (repeat symbol))
                :value-type symbol))

(defcustom general-keymap-aliases
  '((override . general-override-mode-map)
    ((i insert) . evil-insert-state-map)
    ((e emacs) . evil-emacs-state-map)
    ((h hybrid . evil-hybrid-state-map))
    ((n normal) . evil-normal-state-map)
    ((v visual) . evil-visual-state-map)
    ((m motion) . evil-motion-state-map)
    ((o operator) . evil-operator-state-map)
    ((r replace) . evil-replace-state-map)
    ((in inner) . evil-inner-text-objects-map)
    ((out outer) . evil-outer-text-objects-map))
  "An alist for mapping short keymap names to their full names."
  :group 'general
  :type 'general-alist)

(defcustom general-state-aliases
  '((i . insert)
    (e . emacs)
    (h . hybrid)
    (n . normal)
    (v . visual)
    (m . motion)
    (o . operator)
    (r . replace))
  "An alist for mapping short state names to their full names."
  :group 'general
  :type 'general-alist)

;; ** `general-describe-keybindings' Settings
(defcustom general-describe-keybinding-sort-function nil
  "Function used to sort keybindings for `general-describe-keybindings'."
  :group 'general
  :type 'function)

(defcustom general-describe-state-sort-function
  #'general--sort-evil-state-conses
  "Function used to sort the states conses for `general-describe-keybindings'."
  :group 'general
  :type 'function)

(defcustom general-describe-keymap-sort-function nil
  "Function used to sort the keymap conses`general-keybindings' for
`general-describe-keybindings'."
  :group 'general
  :type 'function)

(defcustom general-describe-priority-keymaps
  '(local
    global
    evil-insert-state-map
    evil-emacs-state-map
    evil-hybrid-state-map
    evil-normal-state-map
    evil-visual-state-map
    evil-motion-state-map
    evil-operator-state-map
    evil-replace-state-map
    evil-inner-text-objects-map
    evil-outer-text-objects-map
    evil-ex-search-keymap
    evil-ex-completion-map
    evil-command-window-mode-map
    evil-window-map)
  "Keymaps to print first for `general-describe-keybindings'."
  :group 'general
  :type '(repeat sybmol))

(defcustom general-describe-update-previous-definition 'on-change
  "Whether to update the previous definition when a key is bound.
When set to 'on-change, the previous definition will only be updated when the
definition changes (e.g. re-evaluating a file with keybindings will not affect
the stored previous definition). When set to nil, it will only be updated when
the key was previously unbound."
  :group 'general
  ;; can't think of a use case, but add 'always if requested
  :type '(choice
          (const :tag "When definition has changed" on-change)
          (const :tag "When the key was previously unbound" nil)))

;; * Minor Modes
(defvar general-override-mode-map (make-sparse-keymap)
  "A keymap that will take priority over other minor mode keymaps.
This is only for non-evil keybindings (it won't override keys bound with
`evil-define-key'.")

(define-minor-mode general-override-mode
  "A global minor mode used for key definitions that should override others."
  :lighter ""
  :global t
  :keymap general-override-mode-map)

(defvar-local general-override-local-mode-map (make-sparse-keymap)
  "A keymap that will take priority over other minor mode keymaps.
This keymap is buffer-local and will take precedence over
`general-override-mode-map'. General uses this keymap when creating non-evil
local keybindings.")

(define-minor-mode general-override-local-mode
  "A local minor mode used for key definitions that should override others."
  :lighter ""
  :keymap general-override-local-mode-map)

(defvar-local general-maps-alist
  `((general-override-mode . ,general-override-mode-map))
  "Holds the (mode . keymap) pairs for general's override modes.")
;; not affected by changing major modes
(put 'general-maps-alist 'permanent-local t)

(defvar-local general--maps-alist-updated nil
  "Whether `general-maps-alist' has been set correctly for the current buffer.")
(put 'general-maps-alist 'permanent-local t)

(defun general-override-make-intercept-maps (_sym states)
  "Make intercept keymaps for STATES in `general-override-mode-map'.
This means that keys bound in STATES for `general-override-mode-map' will take
precedence over keys bound in other evil auxiliary maps."
  (with-eval-after-load 'evil
    (dolist (state states)
      (evil-make-intercept-map
       (evil-get-auxiliary-keymap general-override-mode-map state t t)
       state))))

(defcustom general-override-states
  '(insert
    emacs
    hybrid
    normal
    visual
    motion
    operator
    replace)
  "States to make intercept maps for in `general-override-mode-map'.
Note that this uses :set, meaning that if you want to change the value, you
should either set it using customize (e.g. `customize-set-variable') or set it
before loading general if using `setq'."
  :group 'general
  :type '(repeat general-state)
  :set #'general-override-make-intercept-maps)

(defun general--update-maps-alist ()
  "Update `general-maps-alist' for override modes.
This is necessary to ensure `general-override-local-mode-map' is the buffer's
local version."
  (setq general-maps-alist
        `((general-override-local-mode . ,general-override-local-mode-map)
          (general-override-mode . ,general-override-mode-map))
        general--maps-alist-updated t))

(cl-pushnew 'general-maps-alist emulation-mode-map-alists)

;; * General Helpers
(defun general--unalias (symbol &optional statep)
  "Return the full keymap or state name associated with SYMBOL.
If STATEP is non-nil, check `general-state-aliases' instead of
`general-keymap-aliases'."
  (let ((match
         (cdr (cl-assoc symbol
                        (if statep
                            general-state-aliases
                          general-keymap-aliases)
                        ;; test-fn is new to assoc in 26.1
                        :test (lambda (symbol key)
                                (or (eq symbol key)
                                    (ignore-errors (memq symbol key))))))))
    (or match symbol)))

;; don't want to reuse `general--unalias' since the user can alter
;; `general-keymap-aliases'
(defun general--evil-keymap-for-state (state)
  "Return a symbol corresponding to the global evil keymap for STATE."
  (intern (concat "evil-" (symbol-name state) "-state-map")))

(defun general--concat (nokbd &rest keys)
  "Concatenate the strings in KEYS.
If `general-implicit-kbd' is non-nil, interleave the strings in KEYS with
spaces; unless NOKBD is non-nil, apply (kbd ...) to the result. If
`general-implicit-kbd' is nil, just concatenate the keys."
  (setq keys (remove nil keys))
  (if general-implicit-kbd
      (let ((keys (mapconcat (lambda (x)
                               (if (vectorp x)
                                   (key-description x)
                                 x))
                             keys " ")))
        (if nokbd
            keys
          (kbd keys)))
    (apply #'concat keys)))

(defun general--apply-prefix-and-kbd (prefix maps)
  "Prepend the PREFIX sequence to all the keys that are strings in MAPS.
Also apply (kbd ...) to key and definition strings if `general-implicit-kbd' is
non-nil."
  (setq prefix (or prefix ""))
  (cl-loop for (key def) on maps by 'cddr
           collect (general--concat nil prefix key)
           and collect def))

;; http://endlessparentheses.com/define-context-aware-keys-in-emacs.html
(defun general--maybe-apply-predicate (predicate def)
  "Apply PREDICATE to DEF.
If PREDICATE is nil just return DEF."
  (if predicate
      `(menu-item
        "" nil
        :filter (lambda (&optional _)
                  (when ,predicate
                    ',def)))
    def))

(defun general--lookup-key (state keymap key &optional minor-mode-p)
  "Return the current definition for STATE, KEYMAP, and KEY."
  (when key
    (let ((keymap (general--parse-keymap state keymap minor-mode-p)))
      (when keymap
        (let ((def (lookup-key keymap key)))
          (if (and (numberp def) (= def 1))
              nil
            def))))))

(defun general--record-keybindings (keymap state maps &optional minor-mode-p)
  "For KEYMAP and STATE, add MAPS to `general-keybindings'.
If KEYMAP is \"local\", add MAPS to `general-local-keybindings.' For non-evil
keybindings, STATE will be nil. Duplicate keys will be replaced with the new
ones. MINOR-MODE-P should be non-nil when keymap corresponds to a minor-mode
name (as used with `evil-define-minor-mode-key') as opposed to a keymap name."
  (if (and state (not (featurep 'evil)))
      (with-eval-after-load 'evil
        (general--record-keybindings keymap state maps minor-mode-p))
    (let* (keys
           (maps (cl-loop
                  for (key new-def _orig-def) on maps by 'cl-cdddr
                  collect
                  (list key
                        new-def
                        (let* ((current-def (general--lookup-key
                                             state keymap key minor-mode-p))
                               ;; none of these will fail if nil
                               (keymap-cons (assq keymap general-keybindings))
                               (state-cons (assq state (cdr keymap-cons)))
                               (mapping (cl-find key (cdr state-cons)
                                                 :test #'equal :key #'car))
                               (previous-def (cl-caddr mapping)))
                          (if (or
                               (and current-def (not previous-def))
                               (and general-describe-update-previous-definition
                                    (not (equal new-def current-def))))
                              current-def
                            previous-def)))
                  do (push key keys))))
      (cond ((eq keymap 'local)
             (unless (assq state general-local-keybindings)
               (add-to-list 'general-local-keybindings (list state)))
             (let ((state-cons (assq state general-local-keybindings)))
               (setcdr state-cons
                       ;; remove old duplicate keys
                       (cl-remove-duplicates (append (cdr state-cons) maps)
                                             :key #'car
                                             :test #'equal))))
            (t
             (unless (assq keymap general-keybindings)
               (add-to-list 'general-keybindings (list keymap)))
             (unless (assq state (assq keymap general-keybindings))
               (setcdr (assq keymap general-keybindings)
                       (append (cdr (assq keymap general-keybindings))
                               (list (list state)))))
             (let ((state-cons (assq state (assq keymap general-keybindings))))
               (setcdr state-cons
                       (cl-remove-duplicates (append (cdr state-cons) maps)
                                             :key #'car
                                             :test #'equal))))))))

;; don't force non-evil user to require evil for one function
(defun general--delay (condition form hook &optional append local name)
  "Execute FORM when CONDITION becomes true, checking with HOOK.
NAME specifies the name of the entry added to HOOK. If APPEND is
non-nil, the entry is appended to the hook. If LOCAL is non-nil,
the buffer-local value of HOOK is modified.

This is `evil-delay'."
  (declare (indent 2))
  (if (and (not (booleanp condition)) (eval condition))
      (eval form)
    (let* ((name (or name (format "general-delay-form-in-%s" hook)))
           (fun (make-symbol name))
           (condition (or condition t)))
      (fset fun `(lambda (&rest args)
                   (when ,condition
                     (remove-hook ',hook #',fun ',local)
                     ,form)))
      (put fun 'permanent-local-hook t)
      (add-hook hook fun append local))))

(defun general--getf (def fallback-plist keyword)
  "From DEF or FALLBACK-PLIST get the corresponding value for KEYWORD.
FALLBACK-PLIST will be checked when KEYWORD does not exist in DEF (not in cases
where it is explicitly specified as nil). If DEF isn't a general extended
definition, only check in FALLBACK-PLIST."
  (if (general--extended-def-p def)
      (cl-getf def keyword
               (cl-getf fallback-plist keyword))
    (cl-getf fallback-plist keyword)))

(defun general--getf2 (plist keyword1 keyword2)
  "Check in PLIST for either KEYWORD1 or KEYWORD2."
  (or (cl-getf plist keyword1)
      (cl-getf plist keyword2)))

(declare-function evil-get-minor-mode-keymap "evil-core")
(declare-function evil-state-property "evil-common")
(declare-function evil-get-auxiliary-keymap "evil-core")
(cl-defun general--parse-keymap (state keymap &optional minor-mode-p)
  "Transform STATE and the symbol KEYMAP into the appropriate keymap.
'local  - Return `general-override-local-map' or the evil local keymap
'global - Return (current-global-map) or the corresponding evil auxiliary map
else    - Return (symbol-value keymap) or the corresponding evil auxiliary map

Note that if STATE is specified, evil needs to be installed and will be
required."
  (setq keymap (cl-case keymap
                 (global (current-global-map))
                 (local 'local)
                 (t (symbol-value keymap))))
  (if state
      (if (require 'evil nil t)
          (cond (minor-mode-p
                 (evil-get-minor-mode-keymap state keymap))
                ((eq keymap 'local)
                 (evil-state-property state :local-keymap t))
                (t
                 ;; NOTE: this differs from `evil-define-key*'
                 ;; https://github.com/emacs-evil/evil/issues/709
                 (evil-get-auxiliary-keymap keymap state t t)))
        (error "Evil is required if state is specified"))
    (if (eq keymap 'local)
        general-override-local-mode-map
      keymap)))

(defun general--remove-keyword-args (rest)
  "Remove all keyword arguments from the list REST.
Return a list of the altered REST list and a list of the removed keyword
arguments. The order of arguments will be preserved."
  (cl-loop for (key value) on rest by 'cddr
           if (keywordp key)
           collect key into kargs
           and collect value into kargs
           else
           collect key into args
           and collect value into args
           finally (return (list args kargs))))

;; * Extended Key Definition Language
(defvar general-extended-def-keywords '(:which-key :wk)
  "Extra keywords that are valid in an extended definition.")

(defvar which-key-replacement-alist)
(defun general--add-which-key-replacement (mode replacement)
  (let* ((mode-match (assq mode which-key-replacement-alist))
         (mode-alist (cdr mode-match)))
    (cond (mode
           (push replacement mode-alist)
           (if mode-match
               (setcdr mode-match mode-alist)
             (push (cons mode mode-alist)
                   which-key-replacement-alist)))
          (t
           (push replacement which-key-replacement-alist)))))

(defvar which-key--prefix-title-alist)
(defun general--add-which-key-title-prefix (mode keys title-prefix)
  (let* ((mode-match (assq mode which-key--prefix-title-alist))
         (title-mode-alist (cdr mode-match))
         (title-cons (cons keys title-prefix)))
    (cond (mode
           (push title-cons title-mode-alist)
           (if mode-match
               (setcdr mode-match
                       title-mode-alist)
             (push (cons mode title-mode-alist)
                   which-key--prefix-title-alist)))
          (t
           (push title-cons which-key--prefix-title-alist)))))

(defun general--remove-map (keymap)
  "Remove \"-map\" from the symbol KEYMAP." ;
  (intern (replace-regexp-in-string "-map$" "" (symbol-name keymap))))

(defun general-extended-def-:which-key (_state keymap keys def kargs)
  "Add a which-key description for KEY.
If :major-modes is specified in DEF, add the description for the corresponding
major mode. KEY should not be in the kbd format (kbd should have already been
run on it)."
  (with-eval-after-load 'which-key
    (let* ((wk (general--getf2 def :which-key :wk))
           (major-modes (general--getf def kargs :major-modes))
           (keymaps (cl-getf kargs :keymaps))
           ;; index of keymap in :keymaps
           (keymap-index (cl-dotimes (ind (length keymaps))
                           (when (eq (nth ind keymaps) keymap)
                             (cl-return-from nil ind))))
           (mode (let ((mode (if (and major-modes (listp major-modes))
                                 (nth keymap-index major-modes)
                               major-modes)))
                   (if (eq mode t)
                       (general--remove-map keymap)
                     mode)))
           (keys (key-description keys))
           (keys-regexp (concat (when (general--getf def kargs :wk-full-keys)
                                  "\\`")
                                (regexp-quote keys)
                                "\\'"))
           (prefix (cl-getf kargs :prefix))
           (binding (or (general--getf2 def :def :prefix-command)
                        (when (and prefix
                                   (string= keys prefix))
                          (cl-getf kargs :prefix-command))))
           (replacement (cond ((stringp wk)
                               (cons nil wk))
                              (t
                               wk)))
           (match/replacement
            (cons
             (cons (when (general--getf def kargs :wk-match-keys)
                     keys-regexp)
                   (when (and (general--getf def kargs :wk-match-binding)
                              binding
                              (symbolp binding))
                     (symbol-name binding)))
             replacement)))
      (general--add-which-key-replacement mode match/replacement)
      (when (and (consp replacement)
                 ;; lambda
                 (not (functionp replacement)))
        (general--add-which-key-title-prefix
         mode keys (cdr replacement))))))

(defalias 'general-extended-def-:wk #'general-extended-def-:which-key)

(defun general--maybe-autoload-keymap (state keymap def kargs)
  "Return either a keymap or a function that serves as an \"autoloaded\" keymap.
A created function will bind the keys it was invoked with in STATE and KEYMAP to
the keymap specified in DEF with :keymap and then act as if it was originally
bound to that keymap (subsequent keys will be looked up in the keymap). KARGS or
DEF should contain the package in which the keymap is created (as specified
with :package). If the keymap already exists, it will simply be returned."
  (let ((bind-to-keymap (cl-getf def :keymap))
        (package (general--getf def kargs :package)))
    (if (boundp bind-to-keymap)
        (symbol-value bind-to-keymap)
      (unless package
        (error "In order to \"autoload\" a keymap, :package must be specified"))
      `(lambda ()
         (interactive)
         (unless (or (featurep ',package)
                     (require ',package nil t))
           (error (concat "Failed to load package: " (symbol-name ',package))))
         (unless (and (boundp ',bind-to-keymap)
                      (keymapp (symbol-value ',bind-to-keymap)))
           (error (format "A keymap called %s is not defined in the %s package"
                          ',bind-to-keymap ',package)))
         (let ((keys (this-command-keys))
               (general-implicit-kbd nil))
           (general-define-key
            :states ',state
            :keymaps ',keymap
            keys ,bind-to-keymap)
           (setq prefix-arg current-prefix-arg
                 unread-command-events
                 (mapcar (lambda (ev) (cons t ev))
                         (listify-key-sequence keys))))))))

(defun general--extended-def-p (def)
  "Return whether DEF is an extended definition."
  (and (listp def)
       (not (keymapp def))
       ;; lambda
       (not (functionp def))
       (not (eq (car def) 'menu-item))
       (cl-some #'keywordp def)))

(defun general--parse-def (state keymap key def kargs)
  "Rewrite DEF into a valid definition.
This function will execute the actions specified in an extended definition and
apply a predicate if there is one."
  (cond ((general--extended-def-p def)
         (unless (keywordp (car def))
           (setq def (cons :def def)))
         (dolist (keyword general-extended-def-keywords)
           (when (cl-getf def keyword)
             (funcall (intern (concat "general-extended-def-"
                                      (symbol-name keyword)))
                      state keymap key def kargs)))
         (cond ((cl-getf def :ignore)
                ;; just for side effects (e.g. which-key description for prefix)
                ;; return something that isn't a valid definition
                :ignore)
               ((cl-getf def :keymap)
                ;; bind or autoload
                (general--maybe-autoload-keymap state keymap def kargs))
               (t
                (let ((it (cl-getf def :prefix-command)))
                  (when it
                    (define-prefix-command it
                      (cl-getf def :prefix-map)
                      (cl-getf def :prefix-name))))
                (general--maybe-apply-predicate
                 (general--getf def kargs :predicate)
                 (general--getf2 def :def :prefix-command)))))
        (t
         ;; not an extended definition
         (general--maybe-apply-predicate (cl-getf kargs :predicate) def))))

(defun general--parse-maps (state keymap maps kargs)
  "Rewrite MAPS so that the definitions are bindable.
This includes possibly parsing extended definitions. MAPS will be altered to
turn key binding pairs into triples in the form of (key parsed-def original-def)
where parsed-def is the bindable form and original-def is the unaltered
form (e.g. an extended definition)."
  (let (def2)
    (cl-loop for (key def) on maps by 'cddr
             do (setq def2 (general--parse-def state keymap key def kargs))
             unless (eq def2 :ignore)
             collect key
             and collect (if (and general-implicit-kbd
                                  (stringp def2))
                             (kbd def2)
                           def2)
             and collect def)))

;; * Helper Key Definers
(defun general--emacs-local-set-key (key func)
  "Bind KEY to FUNC for only the current buffer.
This will automatically turn on `general-override-local-mode' and update
`general-maps-alist'."
  (general-override-local-mode)
  (unless general--maps-alist-updated
    (general--update-maps-alist))
  (define-key general-override-local-mode-map key func))

(defun general--emacs-define-key (keymap &rest maps)
  "A wrapper for `define-key' and general's `general--emacs-local-set-key'.
KEYMAP determines which keymap the MAPS will be defined in. When KEYMAP is
is 'local, the MAPS will be bound only in the current buffer. MAPS is any
number of paired keys and commands"
  (declare (indent 1))
  (while maps
    (if (eq keymap 'local)
        (general--emacs-local-set-key (pop maps) (pop maps))
      (define-key keymap (pop maps) (pop maps)))))

(declare-function evil-local-set-key "evil-core")
(declare-function evil-define-key* "evil-core")
(defun general--evil-define-key (state keymap key def)
  "A wrapper for `evil-define-key' and `evil-local-set-key'.
In STATE and KEYMAP, bind KEY to DEF. `evil-local-set-key' is used when
KEYMAP is 'local."
  (declare (indent defun))
  (with-eval-after-load 'evil
    (if (eq keymap 'local)
        (evil-local-set-key state key def)
      (evil-define-key* state keymap key def))))

(declare-function evil-define-minor-mode-key "evil-core")
(defun general-minor-mode-define-key (state mode key def _orig-def _kargs)
  "A wrapper for `evil-define-minor-mode-key'."
  (with-eval-after-load 'evil
    (evil-define-minor-mode-key state mode key def)))

(declare-function lispy-define-key "lispy")
(defun general-lispy-define-key (_state keymap key def orig-def kargs)
  "A wrapper for `lispy-define-key'."
  (with-eval-after-load 'lispy
    (let* ((keymap (general--parse-keymap nil keymap))
           (key (key-description key))
           (plist (general--getf orig-def kargs :lispy-plist)))
      (lispy-define-key keymap key def plist))))

(declare-function worf-define-key "worf")
(defun general-worf-define-key (_state keymap key def orig-def kargs)
  "A wrapper for `worf-define-key'."
  (with-eval-after-load 'worf
    (let* ((keymap (general--parse-keymap nil keymap))
           (key (key-description key))
           (plist (general--getf orig-def kargs :worf-plist)))
      (worf-define-key keymap key def plist))))

(declare-function lpy-define-key "lpy")
(defun general-lpy-define-key (_state keymap key def _orig-def _kargs)
  "A wrapper for `lpy-define-key'."
  (with-eval-after-load 'lpy
    (let* ((keymap (general--parse-keymap nil keymap))
           (key (key-description key)))
      (lpy-define-key keymap key def))))

(defun general--define-key-dispatch (state keymap maps kargs)
  "In STATE (if non-nil) and KEYMAP, bind MAPS.
MAPS is composed of triplets of (key parsed-def original-def). This function
determines the appropriate base definer function to use based whether :definer
is present in original-def or KARGS or whether STATE is non-nil."
  (while maps
    (let* ((key (pop maps))
           (def (pop maps))
           (orig-def (pop maps))
           (definer (general--getf orig-def kargs :definer)))
      (if definer
          (funcall (intern (format "general-%s-define-key"
                                   (symbol-name definer)))
                   state keymap key def orig-def kargs)
        ;; purposely keeping state nil for now
        ;; TODO could potentially eliminate --(emacs|evil)-define-key in the
        ;; future; evil-define-key* (keymap prompt) and --emacs-local-set-key
        ;; (turning on minor mode) do additional things that would need to be
        ;; replicated
        ;; If do this, eval-after-load will become necessary if state specified
        (let ((keymap (if (eq keymap 'local)
                          'local
                        (general--parse-keymap nil keymap))))
          (if state
              (general--evil-define-key state keymap key def)
            (general--emacs-define-key keymap key def)))))))

(defun general--define-key
    (states keymap maps non-normal-maps global-maps kargs)
  "A helper function for `general-define-key'.
Choose based on STATES and KEYMAP which of MAPS, NON-NORMAL-MAPS, and
GLOBAL-MAPS to use for the keybindings. This function will rewrite extended
definitions, add predicates when applicable, and then choose the base function
to bind the keys with `general--define-key-dispatch'."
  ;; TODO is it actually necessary to use macros here? could I use free
  ;; variables in functions without issues?
  (cl-macrolet ((defkeys (maps)
                  `(let ((maps (general--parse-maps state keymap ,maps kargs))
                         (keymap keymap))
                     ;; NOTE: minor-mode is not a definition-local definer
                     (general--record-keybindings keymap state maps
                                                  (eq (cl-getf kargs :definer)
                                                      'minor-mode))
                     (general--define-key-dispatch state keymap maps kargs)))
                (def-pick-maps (non-normal-p)
                  `(progn
                     (cond ((and non-normal-maps ,non-normal-p)
                            (defkeys non-normal-maps))
                           ((and global-maps ,non-normal-p))
                           (t
                            (defkeys maps)))
                     (defkeys global-maps))))
    (if states
        (dolist (state states)
          (def-pick-maps (memq state general-non-normal-states)))
      (let (state)
        (def-pick-maps (memq keymap
                             (mapcar #'general--evil-keymap-for-state
                                     general-non-normal-states)))))))

;; * Functions With Keyword Arguments
;;;###autoload
(cl-defun general-define-key
    (&rest maps &key
           definer
           (states general-default-states)
           (keymaps general-default-keymaps)
           (prefix general-default-prefix)
           (non-normal-prefix general-default-non-normal-prefix)
           (global-prefix general-default-global-prefix)
           infix
           prefix-command
           prefix-map
           prefix-name
           predicate
           ;; for extended definitions only
           package
           major-modes
           (wk-match-keys t)
           (wk-match-binding t)
           (wk-full-keys t)
           ;; for custom key definers
           lispy-plist
           worf-plist
           &allow-other-keys)
  "The primary key definition function provided by general.

PREFIX corresponds to a prefix key and defaults to none. STATES corresponds to
the evil state(s) to bind the keys in. Non-evil users should not set STATES.
When STATES is non-nil, `evil-define-key*' will be used; otherwise `define-key'
will be used (unless DEFINER is specified). Evil users may also want to leave
STATES nil and set KEYMAPS to a keymap such as `evil-normal-state-map' for
global mappings. KEYMAPS defaults to 'global (`general-default-keymaps' and
`general-default-states' can be changed by the user). There is also 'local,
which create buffer-local keybindings for both evil and non-evil keybindings.
There are other special, user-alterable \"shorthand\" symbols for keymaps and
states (see `general-keymap-aliases' and `general-state-aliases').

Unlike with normal key definitions functions, the keymaps in KEYMAPS should be
quoted (this allows using the keymap name for other purposes, e.g. deferment,
inferring major mode names by removing \"-map\" for which-key, easily storing
the keymap name for use with `general-describe-keybindings', etc.). Note that
STATES and KEYMAPS can either be a list or a single symbol. If any keymap does
not exist, those keybindings will be deferred until the keymap does exist, so
using `eval-after-load' is not necessary with this function.

MAPS consists of paired keys and definitions. Each pair (if not ignored) will be
recorded for later use with `general-describe-keybindings'.

If NON-NORMAL-PREFIX is specified, this prefix will be used for emacs and insert
state keybindings instead of PREFIX. This argument will only have an effect if
'insert and/or 'emacs is one of the STATES or if 'evil-insert-state-map and/or
'evil-emacs-state-map is one of the KEYMAPS. Alternatively, GLOBAL-PREFIX can be
used with PREFIX and/or NON-NORMAL-PREFIX to bind keys in all states under a
specified prefix. Like with NON-NORMAL-PREFIX, GLOBAL-PREFIX will prevent PREFIX
from applying to insert and emacs states. Note that these keywords are only
useful for evil users.

INFIX can be used to append a string to all of the specified prefixes. This is
potentially useful when you are using GLOBAL-PREFIX and/or NON-NORMAL-PREFIX so
that you can sandwich keys in between all the prefixes and the specified keys in
MAPS. This may be particularly useful if you are using default prefixes in a
wrapper so that you can add to them without needing to re-specify all of them.
If none of the other prefix arguments are specified, INFIX will have no effect.

If PREFIX-COMMAND is specified, a prefix keymap/command will be created using
`define-prefix-command' as long as the symbol specified is not already bound (to
ensure that an existing prefix keymap is not overwritten if the
`general-define-key' function is re-evaluated). All prefixes (including the
INFIX key, if specified) will then be bound to PREFIX-COMMAND. PREFIX-MAP and
PREFIX-NAME can additionally be specified and are used as the last two arguments
to `define-prefix-command'.

If DEFINER is specified, a custom key definer will be used. See the README for
more information.

MAJOR-MODES, WK-MATCH-KEYS, WK-MATCH-BINDINGS, and WK-FULL-KEYS are the
corresponding global versions of which-key extended definition keywords. They
will only have an effect for extended definitions that specify :which-key
or :wk. See the section on extended definitions in the README for more
information.

LISPY-PLIST and WORF-PLIST are the corresponding global versions of extended
definition keywords that are used for the corresponding custom DEFINER"
  ;; to silence compiler warning; variables that are later extracted from kargs
  (ignore definer
          predicate
          package
          major-modes
          lispy-plist
          worf-plist)
  (let (non-normal-prefix-maps
        global-prefix-maps
        kargs)
    ;; don't force the user to wrap a single state or keymap in a list
    ;; luckily nil is a list
    (unless (listp states)
      (setq states (list states)))
    (setq states (mapcar (lambda (state) (general--unalias state t))
                         states))
    (unless (listp keymaps)
      (setq keymaps (list keymaps)))
    (setq keymaps (mapcar #'general--unalias keymaps))
    ;; remove keyword arguments from rest var
    (let ((split-maps (general--remove-keyword-args maps)))
      (setq maps (car split-maps)
            ;; order will be preserved; matters for duplicates
            kargs (append
                   ;; should be included even if not manually specified
                   ;; should be first so :keymaps is a list
                   (list :wk-match-keys wk-match-keys
                         :wk-match-binding wk-match-binding
                         :wk-full-keys wk-full-keys
                         ;; needed for matching against :major-modes
                         :keymaps keymaps
                         ;; for consistency; may be useful in future or for user
                         :states states)
                   (cadr split-maps))))
    (when (and prefix-command
               (not (boundp prefix-command)))
      (define-prefix-command prefix-command prefix-map prefix-name))
    ;; TODO reduce code duplication here
    (when non-normal-prefix
      (setq non-normal-prefix-maps
            (general--apply-prefix-and-kbd
             (general--concat t non-normal-prefix infix)
             (append (when prefix-command
                       (list "" prefix-command))
                     maps))))
    (when global-prefix
      (setq global-prefix-maps
            (general--apply-prefix-and-kbd
             (general--concat t global-prefix infix)
             (append (when prefix-command
                       (list "" prefix-command))
                     maps))))
    ;; last so not applying prefix twice
    (setq maps (general--apply-prefix-and-kbd
                (general--concat t prefix infix)
                (append (when prefix-command
                          (list "" prefix-command))
                        maps)))
    (dolist (keymap keymaps)
      (general--delay `(or (memq ',keymap '(local global))
                           (boundp ',keymap))
          `(general--define-key ',states
                                ',keymap
                                ',maps
                                ',non-normal-prefix-maps
                                ',global-prefix-maps
                                ',kargs)
        'after-load-functions t nil
        (format "general-define-key-in-%s" keymap)))))

;;;###autoload
(defmacro general-create-definer (name &rest args)
  "A helper macro to create key definitions functions.
This allows the creation of key definition functions that
will use a certain keymap, evil state, and/or prefix key by default.
NAME will be the function name and ARGS are the keyword arguments that
are intended to be the defaults."
  `(defun ,name (&rest args)
     ;; can still override keywords afterwards (first keyword takes precedence)
     (apply #'general-define-key (append args (list ,@args)))))

;;;###autoload
(defmacro general-emacs-define-key (keymaps &rest args)
  "A wrapper for `general-define-key' that is similar to `define-key'.
It has a positional argument for KEYMAPS (that will not be overridden by a later
:keymaps argument). Besides this, it acts the same as `general-define-key', and
ARGS can contain keyword arguments in addition to keybindings. This can
basically act as a drop-in replacement for `define-key', and unlike with
`general-define-key', KEYMAPS does not need to be quoted."
  (declare (indent 1))
  `(general-define-key
    :keymaps ,(if (and (listp keymaps)
                       (eq (car keymaps) 'quote))
                  `,keymaps
                `',keymaps)
    ,@args))

;;;###autoload
(defmacro general-evil-define-key (states keymaps &rest args)
  "A wrapper for `general-define-key' that is similar to `evil-define-key'.
It has positional arguments for STATES and KEYMAPS (that will not be overridden
by a later :keymaps or :states argument). Besides this, it acts the same as
`general-define-key', and ARGS can contain keyword arguments in addition to
keybindings. This can basically act as a drop-in replacement for
`evil-define-key', and unlike with `general-define-key', KEYMAPS does not need
to be quoted."
  (declare (indent 2))
  `(general-define-key
    :states ,(if (and (listp states)
                      (eq (car states) 'quote))
                 `,states
               `',states)
    :keymaps ,(if (and (listp keymaps)
                       (eq (car keymaps) 'quote))
                  `,keymaps
                `',keymaps)
    ,@args))

(defun general--positional-arg-p (arg)
  "Return whether ARG is a positional argument for a key definer.
Keyword arguments and strings/vectors are not considered positional arguments."
  (and arg
       (or (symbolp arg) (listp arg))
       (not (keywordp arg))))

;;;###autoload
(defmacro general-def (&rest args)
  "General definer that takes a variable number of positional arguments in ARGS.
This macro will act as `general-define-key', `general-emacs-define-key', or
`general-evil-define-key' based on how many of the initial arguments do not
correspond to keybindings."
  (declare (indent defun))
  (let ((pos-args 0))
    (while (general--positional-arg-p (nth pos-args args))
      (cl-incf pos-args))
    (cl-case pos-args
      (0
       `(general-define-key ,@args))
      (1
       `(general-emacs-define-key ,@args))
      (2
       `(general-evil-define-key ,@args)))))

;; * Displaying Keybindings
(defun general--to-string (x)
  "Convert key vector or symbol X to a string."
  (cond ((vectorp x)
         (key-description x))
        ((symbolp x)
         (symbol-name x))
        (t
         x)))

;; these sorting functions assume x != y (which will hold true for
;; `general-keybindings')
(defun general--< (x y)
  "Return t if X is alphabetically less than Y.
Each should be either a string, symbol, or vector. Nil is a special case and is
considered the \"smallest\"."
  (cond ((null x)
         t)
        ((null y)
         nil)
        (t
         (setq x (general--to-string x)
               y (general--to-string y))
         (string< x y))))

(defun general-sort-by-car (list)
  "Sort LIST by comparing the car of each element with `general--<'."
  (cl-sort list #'general--< :key #'car))

(defun general-sort-by-cadr (list)
  "Sort LIST by comparing the cadr of each element with `general--<'."
  (cl-sort list #'general--< :key #'cadr))

(defvar general-describe-evil-states
  '(nil
    insert
    emacs
    hybrid
    normal
    visual
    motion
    operator
    replace)
  "Ordered list of evil states used for `general--evil-state-<'.")

(defun general--evil-state-< (x y)
  "Return t if evil state X should come before state Y.
If X and Y are conses, the first element will be compared. Ordering is based on
`general-describe-evil-states' or the symbol names for states not in the list."
  (let ((xind (cl-position x general-describe-evil-states))
        (yind (cl-position y general-describe-evil-states)))
    (cond ((and (null xind)
                (null yind))
           (general--< x y))
          ((null xind)
           nil)
          ((null yind)
           t)
          (t
           (< xind yind)))))

(defun general--sort-evil-state-conses (state-conses)
  "Sort STATE-CONSES using `general--evil-state-<'."
  (cl-sort state-conses #'general--evil-state-< :key #'car))

(defun general--print-map (map)
  "Print the keybinding MAP."
  (cl-destructuring-bind (key command previous) map
    (princ (format "|=%.50s=|~%.50s~|~%.50s~|\n"
                   (key-description key)
                   command
                   previous))))

(defun general--print-maps-table (maps)
  "Print an org table for MAPS."
  (when general-describe-keybinding-sort-function
    (setq maps (funcall general-describe-keybinding-sort-function maps)))
  (princ "|key|command|previous|\n|-+-|\n")
  (dolist (map maps)
    (general--print-map map))
  (princ "\n"))

(defun general--print-state-heading (state-cons)
  "Print a table and possibly a heading for STATE-CONS."
  (let ((state (car state-cons))
        (maps (cdr state-cons)))
    (unless (null state)
      (princ (capitalize (concat "** " (symbol-name state) " State\n"))))
    (general--print-maps-table maps)))

(defun general--print-keymap-heading (keymap-cons)
  "Print headings and tables for KEYMAP-CONS."
  (let ((keymap (car keymap-cons))
        (state-conses (cdr keymap-cons)))
    (princ (capitalize (concat "* " (symbol-name keymap) " Keybindings\n")))
    (when general-describe-state-sort-function
      (setq state-conses (funcall general-describe-state-sort-function
                                  state-conses)))
    (dolist (state-cons state-conses)
      (general--print-state-heading state-cons))))

(declare-function org-at-heading-p "org")
(declare-function org-table-align "org-table")
(declare-function outline-next-heading "outline")
(defvar org-startup-folded)
;;;###autoload
(defun general-describe-keybindings ()
  "Show all keys that have been bound with general in an org buffer.
Any local keybindings will be shown first followed by global keybindings."
  (interactive)
  (with-output-to-temp-buffer "*General Keybindings*"
    (let* ((keybindings (append
                         (copy-alist general-keybindings)
                         (list (cons 'local general-local-keybindings)))))
      ;; print prioritized keymaps first (if any)
      (dolist (keymap general-describe-priority-keymaps)
        (let ((keymap-cons (assq keymap keybindings)))
          (when keymap-cons
            (general--print-keymap-heading keymap-cons)
            (setq keybindings (assq-delete-all keymap keybindings)))))
      ;; sort the remaining and then print them
      (when general-describe-keymap-sort-function
        (setq keybindings (funcall general-describe-keymap-sort-function
                                   keybindings)))
      (dolist (keymap-cons keybindings)
        (general--print-keymap-heading keymap-cons))))
  (with-current-buffer "*General Keybindings*"
    (let ((org-startup-folded 'showall))
      (org-mode))
    (read-only-mode -1)
    (while (progn
             (while (progn
                      (forward-line)
                      (org-at-heading-p)))
             (org-table-align)
             (outline-next-heading)))
    (goto-char (point-min))
    (read-only-mode)))

;; * Functions/Macros to Aid Key Definition
;; https://emacs.stackexchange.com/questions/6037/emacs-bind-key-to-prefix/13432#13432
;; altered to
;; - allow execution in an arbitrary state and keymap
;; - create a named function with a docstring
;; - handle more edge cases like correctly adding evil repeat info

;; ** Key Simulation
(defvar general--last-simulated-command nil
  "Holds the last simulated command (or nil for incomplete key sequence).")

(defvar general--simulate-next-as-is nil
  "Whether to fake keys unconditionally in the next `general--simulate-keys'.
This is used for testing (but could potentially be useful for a user). Since
`general--simulate-keys' will normally assume it is being run inside a macro
that was manually recorded, this is needed when executing a keyboard macro that
ends up running general--simulate-keys for the first time.")

(declare-function evil-change-state "evil-core")
(defvar evil-no-display)
(defvar evil-state)
(defvar evil-previous-state)
(defvar evil-previous-state-alist)
(defvar evil-next-state)
(defmacro general--save-state (&rest body)
  "Save the current state; execute BODY; restore the state.
This is a combination of `evil-without-display' and `evil-save-state'. It is
necessary to define this directly in general so that it is available when
general is compiled (as evil is an optional dependency and may not be installed
when general is compiled)."
  (declare (indent defun)
           (debug t))
  `(let* ((evil-no-display t)
          (evil-state evil-state)
          (evil-previous-state evil-previous-state)
          (evil-previous-state-alist (copy-tree evil-previous-state-alist))
          (evil-next-state evil-next-state)
          (old-state evil-state)
          (inhibit-quit t)
          (buf (current-buffer)))
     (unwind-protect
         (progn ,@body)
       (when (buffer-live-p buf)
         (with-current-buffer buf
           (evil-change-state old-state))))))

(defun general--key-lookup (state keymap &optional keys)
  "In the keymap for STATE and KEYMAP, look up KEYS.
Return the keymap that corresponds to STATE and KEYMAP. When KEYS is also
specified, and there is a matched command or keymap, also return the matched
command or keymap and the leftover keys. STATE should only be specified when
evil is in use.

Note that if STATE is specified, evil needs to be installed and will be
required."
  (when state
    (or (require 'evil nil t)
        (error "Evil is required if state is specified")))
  (let* ((state (if (eq state t)
                    'emacs
                  state))
         (keymap (if (and keymap state)
                     (evil-get-auxiliary-keymap keymap state)
                   keymap))
         (len (length keys))
         (ind len)
         matched-command
         matched-keymap)
    (while (and (> ind 0) (not (or matched-command matched-keymap)))
      (let* ((key (substring keys 0 ind))
             (match (cond (keymap
                           (lookup-key keymap key))
                          (state
                           ;; this also works fine when evil-local-mode is off
                           (general--save-state
                             (evil-change-state state)
                             (key-binding key)))
                          (t
                           (key-binding key)))))
        (cond ((commandp match)
               (setq matched-command match))
              ((keymapp match)
               (setq matched-keymap match))
              (t
               (cl-decf ind)))))
    (list :keymap keymap
          :match (or matched-command matched-keymap)
          :keys (if (= ind len)
                    nil
                  (substring keys ind len)))))

(defun general--simulate-keys (command keys &optional state keymap no-lookup)
  "Simulate COMMAND followed by KEYS in STATE and KEYMAP.
If COMMAND is nil, just simulate KEYS. If STATE and KEYMAP are nil, simulate the
keys in the current context. When COMMAND is non-nil, STATE and KEYMAP will have
no effect. KEYS should be a string that can be passed to `kbd' or nil, and STATE
should be a quoted evil state. If KEYS is nil, the COMMAND will just be called
interactively.

If COMMAND is nil, KEYS will normally be looked up in the correct context to
determine if any subsequnce corresponds to a command or keymap. If a command is
matched, that command will be called followed by the simulation of any leftover
keys. If a keymap is matched, that keymap will be set to the keymap for the next
command using `set-transient-map'. To simulate the keys as-is without any
lookup, NO-LOOKUP can be specified as non-nil. The reason you might want to do
this is that it can fix repeating behavior for incomplete simulations. For
example, if you simulate \"di\" and then manually type \"w\", repeating will
only work if you specify NO-LOOKUP as non-nil. You generally don't want to use
this option. Lookup is useful in other situation. For example, the repeat
property of matched commands will be used when determining whether or not they
should be recorded."
  (let ((keys (when keys
                (kbd keys))))
    (unless (or command no-lookup)
      (let* ((lookup (general--key-lookup state keymap keys))
             (match (cl-getf lookup :match)))
        (setq command (when (commandp match)
                        match)
              keys (cl-getf lookup :keys)
              keymap (if (keymapp match)
                         match
                       (cl-getf lookup :keymap)))))
    ;; keys not required since a keymap can be a match for keys
    (when (and keymap (not command))
      (set-transient-map keymap))
    (when keys
      ;; only set prefix-arg when keys (otherwise will also affect next command)
      (setq prefix-arg current-prefix-arg)
      (when (or general--simulate-next-as-is
                (not executing-kbd-macro))
        (setq general--simulate-next-as-is nil)
        ;; keys are incorrectly saved as this-command-keys when recording macros
        ;; these keys will be played back, so don't simulate them
        (setq unread-command-events
              (nconc
               ;; force keys to be added to this-command-keys
               ;; this happens normally already for macros but it needs to be
               ;; forced for evil-repeat though, which will only include the
               ;; first key otherwise (ideally no keys would ever be added in
               ;; either case)
               (mapcar (lambda (ev) (cons t ev))
                       (listify-key-sequence keys))
               unread-command-events))))
    (when command
      (let ((this-command command))
        (call-interactively command)))
    (setq general--last-simulated-command command)))

;;;###autoload
(cl-defmacro general-simulate-keys (keys &optional state keymap
                                         no-lookup docstring name)
  "Create a function to simulate KEYS in STATE and KEYMAP.
STATE should only be specified by evil users and can be a quoted evil state or
t (in which case emacs state will be used). When neither STATE or KEYMAP are
specified, the keys will be simulated in the current context. Normally the
generated function will look up KEYS in the correct context to try to match a
command or keymap. To prevent this lookup, NO-LOOKUP can be specified as
non-nil. See the docstring for `general--simulate-keys' for some insight as to
why you might want to use this.

KEYS should be a string given in `kbd' notation. It can also be a list of a
single command followed by a string of the keys to simulate after calling that
command. If DOCSTRING is given, it will replace the automatically generated
docstring. If NAME is given, it will replace the automatically generated
function name. NAME should not be quoted.

The advantages of this over a keyboard macro are as follows:
- The user can control the context in which the keys are simulated
- The user can simulate both a command and keys
- The user can simulate a partial key sequence (e.g. for a keymap)"
  (let* ((command (when (listp keys)
                    (car keys)))
         (keys (if (listp keys)
                   (cadr keys)
                 keys))
         (state (if (eq state t)
                    ''emacs
                  state))
         (name (or name
                   (intern (concat
                            (format "general-simulate-%s"
                                    (if command
                                        (eval command)
                                      ""))
                            (when command
                              "-")
                            (replace-regexp-in-string " " "_" keys)
                            (when state
                              (concat "-in-"
                                      (symbol-name (eval state))
                                      "-state"))
                            (when keymap
                              (concat "-in-"
                                      (symbol-name keymap))))))))
    `(progn
       (eval-after-load 'evil
         '(evil-set-command-property #',name :repeat 'general--simulate-repeat))
       (defun ,name
           ()
         ,(or docstring
              (concat "Simulate "
                      (when command
                        (concat "`"
                                (symbol-name (eval command))
                                "' then "))
                      "'"
                      keys
                      "' in "
                      (cond ((and state keymap)
                             (concat (symbol-name (eval state))
                                     " state in `"
                                     (symbol-name keymap)
                                     "'."))
                            (keymap
                             (concat (symbol-name keymap)
                                     "."))
                            (state
                             (concat (symbol-name (eval state))
                                     " state."))
                            (t
                             "the current context."))))
         (interactive)
         (general--simulate-keys ,command ,keys ,state ,keymap ,no-lookup)))))

(defun general--repeat-abort-p (repeat-prop)
  "Return t if repeat recording should be aborted based on REPEAT-PROP."
  (or (memq repeat-prop (list nil 'abort 'ignore))
      (and (eq repeat-prop 'motion)
           (not (memq evil-state '(insert replace))))))

(declare-function evil-repeat-record "evil-repeat")
(declare-function evil-get-command-property "evil-common")
(declare-function evil-repeat-abort "evil-repeat")
(declare-function evil-this-command-keys "evil-repeat")
(declare-function evil-clear-command-keys "evil-repeat")
(defvar evil-this-register)
(defun general--simulate-repeat (flag)
  "Modified version of `evil-repeat-keystrokes'.
It behaves as normal but will check the repeat property of a simulated command
to determine whether to abort recording."
  (cond ((eq flag 'pre)
         (when evil-this-register
           (evil-repeat-record
            `(set evil-this-register ,evil-this-register))))
        ((eq flag 'post)
         (let* ((command general--last-simulated-command)
                (repeat-prop (evil-get-command-property command :repeat t)))
           (if (and command (general--repeat-abort-p repeat-prop))
               (evil-repeat-abort)
             (evil-repeat-record
              (evil-this-command-keys t))
             (evil-clear-command-keys))))))

;; ** Key Dispatch
(defvar general--last-dispatch-command nil
  "Holds the last command run from a `general-key-dispatch' function.")

(defun general--extend-key-sequence (keys)
  "Read a key and append it to KEYS.
KEYS should be a string given in `kbd' notation."
  (let ((key (read-event)))
    (concat keys
            (when keys
              " ")
            (key-description (if (characterp key)
                                 (char-to-string key)
                               (vector key))))))

;;;###autoload
(cl-defmacro general-key-dispatch
    (fallback-command &rest maps
                      &key timeout inherit-keymap name docstring
                      which-key
                      &allow-other-keys)
  "Create a function that will run FALLBACK-COMMAND or a command from MAPS.
MAPS consists of <key> <command> pairs. If a key in MAPS is matched, the
corresponding command will be run. Otherwise FALLBACK-COMMAND will be run with
the unmatched keys. So, for example, if \"ab\" was pressed, and \"ab\" is not
one of the key sequences from MAPS, the FALLBACK-COMMAND will be run followed by
the simulated keypresses of \"ab\". Prefix arguments will still work regardless
of which command is run. This is useful for binding under non-prefix keys. For
example, this can be used to redefine a sequence like \"cw\" or \"cow\" in evil
but still have \"c\" work as `evil-change'. If TIMEOUT is specified,
FALLBACK-COMMAND will also be run in the case that the user does not press the
next key within the TIMEOUT (e.g. 0.5).

NAME and DOCSTRING are optional keyword arguments. They can be used to replace
the automatically generated name and docstring for the created function and are
potentially useful if you want to create multiple, different commands using the
same FALLBACK-COMMAND (e.g. `self-insert-command').

When INHERIT-KEYMAP is specified, all the keybindings from that keymap will be
inherited in MAPS.

WHICH-KEY can also be specified, in which case the description WHICH-KEY will
replace the command name in the which-key popup. Note that this requires a
version of which-key from after 2016-11-21."
  (declare (indent 1))
  (let ((name (or name (intern (format "general-dispatch-%s"
                                       (eval fallback-command)))))
        ;; remove keyword arguments from maps
        (maps (car (general--remove-keyword-args maps))))
    `(progn
       (eval-after-load 'evil
         '(evil-set-command-property #',name :repeat 'general--dispatch-repeat))
       (when ,which-key
         (with-eval-after-load 'which-key
           (when (boundp 'which-key-replacement-alist)
             (push '((nil . ,(symbol-name name))
                     nil . ,which-key)
                   which-key-replacement-alist))))
       ;; TODO list all of the bound keys in the docstring
       (defun ,name ()
         ,(or docstring (format (concat "Run %s or something else based"
                                        "on the next keypresses.")
                                (eval fallback-command)))
         (interactive)
         (let ((map (make-sparse-keymap))
               (invoked-keys (this-command-keys))
               (timeout ,timeout)
               (inherit-keymap ,inherit-keymap)
               matched-command
               fallback
               char
               timed-out-p)
           (when inherit-keymap
             (set-keymap-parent map inherit-keymap))
           (if general-implicit-kbd
               (general--emacs-define-key map
                 ,@(general--apply-prefix-and-kbd nil maps))
             (general--emacs-define-key map ,@maps))
           (while (progn
                    (if timeout
                        (with-timeout (timeout (setq timed-out-p t))
                          ;; TODO rename char
                          (setq char (general--extend-key-sequence char)))
                      (setq char (general--extend-key-sequence char)))
                    (and (not timed-out-p)
                         (keymapp (lookup-key map (kbd char))))))
           (cond
            ((and (not timed-out-p)
                  (setq matched-command (lookup-key map (kbd char))))
             ;; necessary for evil-this-operator checks because
             ;; evil-define-operator sets evil-this-operator to this-command
             (let ((this-command matched-command))
               (call-interactively matched-command)))
            (t
             (setq matched-command ,fallback-command)
             (general--simulate-keys ,fallback-command char)))
           (setq general--last-dispatch-command matched-command))))))

(defun general--dispatch-repeat (flag)
  "Modified version of `evil-repeat-keystrokes'.
It behaves as normal but will check the repeat property of a simulated command
to determine whether to abort recording."
  (cond ((eq flag 'pre)
         (when evil-this-register
           (evil-repeat-record
            `(set evil-this-register ,evil-this-register))))
        ((eq flag 'post)
         (let ((repeat-prop (evil-get-command-property
                             general--last-dispatch-command
                             :repeat t)))
           (if (general--repeat-abort-p repeat-prop)
               (evil-repeat-abort)
             (evil-repeat-record (evil-this-command-keys t))
             (evil-clear-command-keys))))))

;; ** Predicate Dispatch
(cl-defmacro general-predicate-dispatch
    (fallback-def &rest defs
                  &key docstring
                  &allow-other-keys)
  (declare (indent 1))
  "Create a menu item that will run FALLBACK-DEF or a definition from DEFS.
DEFS consists of <predicate> <definition> pairs. Binding this menu-item to a key
will cause that key to act as the corresponding definition (a command, keymap,
etc) for the first matched predicate. If no predicate is matched FALLBACK-DEF
will be run. When FALLBACK-DEF is nil and no predicates are matched, the keymap
with the next highest precedence for the pressed key will be checked. DOCSTRING
can be specified as a description for the menu item."
  ;; remove keyword arguments from defs and sort defs into pairs
  (let ((defs (cl-loop for (key value) on defs by 'cddr
                       unless (keywordp key)
                       collect (list key value))))
    `'(menu-item
       ,(or docstring "") nil
       :filter (lambda (&optional _)
                 (cond ,@(mapcar (lambda (pred-def)
                                   `(,(car pred-def) ,(cadr pred-def)))
                                 defs)
                       (t ,fallback-def))))))

;; ** Key "Translation"
;;;###autoload
(cl-defun general-translate-keys (state keymap-name
                                        &rest maps
                                        &key destructive
                                        &allow-other-keys)
  "Translate keys in the keymap corresponding to STATE and KEYMAP-NAME.
STATE should be the name of an evil state or nil. KEYMAP-NAME should be a symbol
corresponding to the keymap to make the translations in. MAPS corresponds to a
list of translations (key replacement pairs). For example, specifying \"a\"
\"b\" will bind \"a\" to \"b\"'s definition in the keymap. If DESTRUCTIVE is
non-nil, the keymap will be destructively altered without a backup being
created. If DESTRUCTIVE is nil, a backup of the keymap will be stored on the
initial invocation, and future invocations will always reference the backup
keymap, meaning that invocations are idempotent. On the other hand, if
DESTRUCTIVE is non-nil, calling this function multiple times with \"a\" \"b\"
\"b\" \"a\", for example, would continue to swap and unswap the definitions of
these keys. This means that when DESTRUCTIVE is non-nil, all related
swaps/cycles should be done in the same invocation."
  (declare (indent defun))
  (let* ((keymap (if state
                     (evil-get-auxiliary-keymap
                      (symbol-value keymap-name) state t t)
                   (symbol-value keymap-name)))
         (backup-keymap-name (intern (format "general-%S-%S-state-backup-map"
                                             keymap-name
                                             (or nil 'no))))
         (backup-keymap (if (boundp backup-keymap-name)
                            (symbol-value backup-keymap-name)
                          (copy-keymap keymap))))
    (unless destructive
      (set backup-keymap-name backup-keymap))
    (setq maps (cl-loop for (key replacement) on maps by 'cddr
                        ;; :destructive can be in MAPS
                        unless (keywordp key)
                        collect key
                        and collect (lookup-key (if destructive
                                                    keymap
                                                  backup-keymap)
                                                replacement)))
    (apply #'general-define-key :states state :keymaps keymap-name maps)))

;; * Functions/Macros to Aid Other Configuration
;; ** Hooks
(defun general-add-hook (hooks functions &optional append local)
  "A drop-in replacement for `add-hook'.
HOOKS and FUNCTIONS can be single items or lists."
  (unless (listp hooks)
    (setq hooks (list hooks)))
  (unless (listp functions)
    (setq functions (list functions)))
  (dolist (hook hooks)
    (dolist (func functions)
      (add-hook hook func append local))))

(defun general-remove-hook (hooks functions &optional local)
  "A drop-in replacement for `remove-hook'.
HOOKS and FUNCTIONS can be single items or lists."
  (unless (listp hooks)
    (setq hooks (list hooks)))
  (unless (listp functions)
    (setq functions (list functions)))
  (dolist (hook hooks)
    (dolist (func functions)
      (remove-hook hook func local))))

;; ** Advice
(defun general-add-advice (symbols where functions &optional props)
  "A drop-in replacement for `advice-add'.
SYMBOLS and FUNCTIONS can be single items or lists."
  (unless (listp symbols)
    (setq symbols (list symbols)))
  (unless (listp functions)
    (setq functions (list functions)))
  (dolist (symbol symbols)
    (dolist (func functions)
      (advice-add symbol where func props))))

(defalias 'general-advice-add #'general-add-advice)

(defun general-remove-advice (symbols functions)
  "A drop-in replacement for `advice-remove'.
SYMBOLS and FUNCTIONS can be single items or lists."
  (unless (listp symbols)
    (setq symbols (list symbols)))
  (unless (listp functions)
    (setq functions (list functions)))
  (dolist (symbol symbols)
    (dolist (func functions)
      (advice-remove symbol func))))

(defalias 'general-advice-remove #'general-remove-advice)

;; * Optional Setup
;;;###autoload
(defmacro general-create-vim-definer
    (name keymaps &optional states default-to-states)
  "A helper function to create vim-like wrappers over `general-define-key'.
The function created will be called NAME and will have the keymaps default to
KEYMAPS or the states default to STATES (both should be quoted). If
DEFAULT-TO-STATES is non-nil, :states STATES will be used. Otherwise :keymaps
KEYMAPS will be used. This can be overriden later by setting the global
`general-vim-definer-default' option."
  `(defmacro ,name (&rest args)
     ,(format
       "A wrapper for `general-def'.

It has one the following defaults depending on `general-vim-definer-default':
:keymaps
%s

:states
%s

When `general-vim-definer-default' is nil, default to setting %s.

If the default :states is nil,the :keymaps default will be used no matter what.
If the default :states is non-nil and the user specifies keymaps (with :keymaps
or the positional argument), the default :states will be used."
       keymaps
       states
       (if default-to-states
           ":states"
         ":keymaps"))
     (let ((default-to-states
             (cl-case general-vim-definer-default
               (states t)
               (keymaps nil)
               (t ,default-to-states))))
       `(general-def ,@args
          ,@(if (and ,states
                     (or default-to-states
                         (cl-getf args :keymaps)
                         (general--positional-arg-p (car args))))
                '(:states ,states)
              '(:keymaps ,keymaps))))))

;;;###autoload
(defmacro general-create-dual-vim-definer
    (name states &optional default-to-states)
  "Like `general-create-vim-definer', create a \"vim definer\" called NAME.
Only the short names in the STATES list need to be specified, but this will only
work for valid evil states."
  `(general-create-vim-definer
    ,name
    ;; could alternatively just do ,states (difference is the docstring)
    ',(let ((states (eval states)))
        (if (listp states)
            (mapcar #'general--evil-keymap-for-state states)
          (general--evil-keymap-for-state states)))
    ,states
    ,default-to-states))

;;;###autoload
(defmacro general-evil-setup (&optional short-names default-to-states)
  "Set up some basic equivalents for vim mapping functions.
This creates global key definition functions for the evil states.
Specifying SHORT-NAMES as non-nil will create non-prefixed function
aliases such as `nmap' for `general-nmap'."
  `(progn
     (general-create-dual-vim-definer general-imap 'insert ,default-to-states)
     (general-create-dual-vim-definer general-emap 'emacs ,default-to-states)
     (general-create-dual-vim-definer general-nmap 'normal ,default-to-states)
     (general-create-dual-vim-definer general-vmap 'visual ,default-to-states)
     (general-create-dual-vim-definer general-mmap 'motion ,default-to-states)
     (general-create-dual-vim-definer general-omap 'operator ,default-to-states)
     (general-create-dual-vim-definer general-rmap 'replace ,default-to-states)
     ;; these two don't have corresponding states
     (general-create-vim-definer general-itomap 'evil-inner-text-objects-map)
     (general-create-vim-definer general-otomap 'evil-outer-text-objects-map)
     (general-create-dual-vim-definer general-iemap
                                      '(insert emacs)
                                      ,default-to-states)
     (general-create-dual-vim-definer general-nvmap
                                      '(normal visual)
                                      ,default-to-states)
     (general-create-vim-definer general-tomap
                                 '(evil-outer-text-objects-map
                                   evil-inner-text-objects-map))
     (when ,short-names
       (defalias 'imap #'general-imap)
       (defalias 'emap #'general-emap)
       (defalias 'nmap #'general-nmap)
       (defalias 'vmap #'general-vmap)
       (defalias 'mmap #'general-mmap)
       (defalias 'omap #'general-omap)
       (defalias 'rmap #'general-rmap)
       (defalias 'itomap #'general-itomap)
       (defalias 'otomap #'general-otomap)
       (defalias 'iemap #'general-iemap)
       (defalias 'nvmap #'general-nvmap)
       (defalias 'tomap #'general-tomap))))

;; * Use-package Integration
(with-eval-after-load 'use-package
  (declare-function use-package-concat "use-package")
  (declare-function use-package-process-keywords "use-package")
  (declare-function use-package-sort-keywords "use-package")
  (declare-function use-package-plist-maybe-put "use-package")
  (declare-function use-package-plist-append "use-package")
  (defvar use-package-keywords)
  (setq use-package-keywords
        ;; should go in the same location as :bind
        ;; adding to end may not cause problems, but see issue #22
        (cl-loop for item in use-package-keywords
                 if (eq item :bind-keymap*)
                 collect :bind-keymap* and collect :general
                 else
                 ;; don't add duplicates
                 unless (eq item :general)
                 collect item))
  (defun use-package-normalize/:general (_name _keyword args)
    "Return ARGS."
    args)
  (defun general--extract-symbol (def)
    "Extract autoloadable symbol from DEF, a normal or extended definition."
    (when def
      (if (general--extended-def-p def)
          (let ((first (car def))
                (inner-def (cl-getf def :def)))
            (cond ((symbolp inner-def)
                   inner-def)
                  ((and (symbolp first)
                        (not (keywordp first)))
                   first)))
        (when (symbolp def)
          def))))
  (declare-function general--extract-symbol "general")
  (defun use-package-handler/:general (name _keyword arglists rest state)
    "Use-package handler for :general."
    (let* ((sanitized-arglist
            ;; combine arglists into one without function names or
            ;; positional arguments
            (let (result)
              (dolist (arglist arglists result)
                (while (general--positional-arg-p (car arglist))
                  (setq arglist (cdr arglist)))
                (setq result (append result arglist)))))
           (commands
            (cl-loop for (key def) on sanitized-arglist by 'cddr
                     when (and (not (keywordp key))
                               (not (null def))
                               (ignore-errors
                                 (setq def (eval def))
                                 (setq def (general--extract-symbol def))))
                     collect def)))
      (use-package-concat
       (use-package-process-keywords name
         (use-package-sort-keywords
          (use-package-plist-maybe-put rest :defer t))
         (use-package-plist-append state :commands commands))
       `((ignore ,@(mapcar (lambda (arglist)
                             ;; Note: prefix commands are not valid functions
                             (if (or (functionp (car arglist))
                                     (macrop (car arglist)))
                                 `(,@arglist :package ',name)
                               `(general-def
                                  ,@arglist
                                  :package ',name)))
                           arglists)))))))

;; * Key-chord "Integration"
(defun general-chord (keys)
  "Rewrite the string KEYS into a valid key-chord vector."
  ;; taken straight from key-chord.el
  (if (/= 2 (length keys))
      (error "Key-chord keys must have two elements"))
  ;; Exotic chars in a string are >255 but define-key wants 128..255 for those
  (let ((key1 (logand 255 (aref keys 0)))
        (key2 (logand 255 (aref keys 1))))
    (vector 'key-chord key1 key2)))

(provide 'general)
;;; general.el ends here
