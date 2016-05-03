;;; general-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (general-evil-setup general-evil-define-key general-emacs-define-key
;;;;;;  general-create-definer general-define-key) "general" "general.el"
;;;;;;  (22313 2389 447832 559000))
;;; Generated autoloads from general.el

(autoload 'general-define-key "general" "\
The primary key definition function provided by general.

PREFIX corresponds to a prefix key and defaults to none. STATES corresponds to
the evil state(s) to bind the keys in. Non-evil users should not set STATES.
When STATES is non-nil, `evil-define-key' will be used. Otherwise `define-key'
will be used. Evil users may also want to leave STATES nil and set KEYMAPS to
a keymap such as `evil-normal-state-map' for global mappings. KEYMAPS defaults
to `global-map'. Note that STATES and KEYMAPS can either be a list or a single
symbol. If any keymap does not exist, the keybindings will be deferred until
the keymap does exist, so using `eval-after-load' is not necessary with this
function.

If NON-NORMAL-PREFIX is specified, this prefix will be used for emacs and insert
state keybindings instead of PREFIX. This argument will only have an effect if
'insert and/or 'emacs is one of the STATES or if 'evil-insert-state-map and/or
'evil-emacs-state-map is one of the KEYMAPS. Alternatively, GLOBAL-PREFIX can be
used with PREFIX and/or NON-NORMAL-PREFIX to bind keys in all states under a
specified prefix. Like with NON-NORMAL-PREFIX, GLOBAL-PREFIX will prevent PREFIX
from applying to insert and emacs states. Note that these keywords are only
useful for evil users.

Unlike with normal key definitions functions, the keymaps in KEYMAPS should be
quoted (this makes it easy to check if there is only one keymap instead of a
list of keymaps).

\(fn &rest MAPS &key (prefix general-default-prefix) (non-normal-prefix general-default-non-normal-prefix) (global-prefix general-default-global-prefix) (states general-default-states) (keymaps general-default-keymaps) (predicate) &allow-other-keys)" nil nil)

(autoload 'general-create-definer "general" "\
A helper macro to create key definitions functions.
This allows the creation of key definition functions that
will use a certain keymap, evil state, and/or prefix key by default.
NAME will be the function name and ARGS are the keyword arguments that
are intended to be the defaults.

\(fn NAME &rest ARGS)" nil t)

(autoload 'general-emacs-define-key "general" "\
A wrapper for `general-define-key' that is similar to `define-key'.
It has a positional argument for KEYMAPS. It acts the same as
`general-define-key', and ARGS can contain keyword arguments in addition to
keybindings.

\(fn KEYMAPS &rest ARGS)" nil nil)

(put 'general-emacs-define-key 'lisp-indent-function '1)

(autoload 'general-evil-define-key "general" "\
A wrapper for `general-define-key' that is similar to `evil-define-key'.
It has positional arguments for STATES and KEYMAPS. It acts the same as
`general-define-key', and ARGS can contain keyword arguments in addition to
keybindings.

\(fn STATES KEYMAPS &rest ARGS)" nil nil)

(put 'general-evil-define-key 'lisp-indent-function 'defun)

(autoload 'general-evil-setup "general" "\
Set up some basic equivalents for vim mapping functions.
This creates global key definition functions for the evil states.
Specifying SHORT-NAMES as non-nil will create non-prefixed function
aliases such as `nmap' for `general-nmap'.

\(fn &optional SHORT-NAMES)" nil nil)

;;;***

;;;### (autoloads nil nil ("general-pkg.el") (22313 2389 574605 457000))

;;;***

(provide 'general-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; general-autoloads.el ends here
