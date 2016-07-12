;;; dtrt-indent-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "dtrt-indent" "dtrt-indent.el" (22404 49275
;;;;;;  0 0))
;;; Generated autoloads from dtrt-indent.el

(defvar dtrt-indent-mode nil "\
Non-nil if Dtrt-Indent mode is enabled.
See the command `dtrt-indent-mode' for a description of this minor mode.")

(custom-autoload 'dtrt-indent-mode "dtrt-indent" nil)

(autoload 'dtrt-indent-mode "dtrt-indent" "\
Toggle dtrt-indent mode.
With no argument, this command toggles the mode.  Non-null prefix
argument turns on the mode.  Null prefix argument turns off the
mode.

When dtrt-indent mode is enabled, the proper indentation offset
and `indent-tabs-mode' will be guessed for newly opened files and
adjusted transparently.

\(fn &optional ARG)" t nil)

(defvar dtrt-indent-mode nil "\
Toggle adaptive indentation mode.
Setting this variable directly does not take effect;
use either \\[customize] or the function `dtrt-indent-mode'.")

(custom-autoload 'dtrt-indent-mode "dtrt-indent" nil)

;;;***

;;;### (autoloads nil nil ("dtrt-indent-diag.el" "dtrt-indent-pkg.el")
;;;;;;  (22404 49276 176586 700000))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; dtrt-indent-autoloads.el ends here
