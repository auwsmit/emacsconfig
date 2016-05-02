;; Enable package.el, the built-in package manager
(package-initialize)

;; Package repositories
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			 ("melpa" . "http://melpa.org/packages/")
			 ("melpa-stable" . "http://stable.melpa.org/packages/")))

;; TODO: Keep all packages in dotfiles for safety/convenience.
;; * Can I still use use-package to load local packages?
;;   * if YES: cool.
;;   * if NO: What needs to be revised?

;; Automatically install use-package
;; for automatic package installation.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

(provide 'setup-package)
