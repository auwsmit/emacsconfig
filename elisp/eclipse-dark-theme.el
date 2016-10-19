;;; eclipse-dark-theme.el --- Theme based on Eclipse-Dark circa 2010

;; Copyright (C) 2015 Oleh Krehel

;; Author: Oleh Krehel <ohwoeowho@gmail.com>
;; URL: https://github.com/abo-abo/eclipse-dark-theme
;; Version: 0.1.0
;; Keywords: themes

;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This theme assumes light background.  To load it, use:
;;
;;     (require 'eclipse-dark-theme)

;;; Code:

(deftheme eclipse-dark
    "Color theme from Eclipse-Dark.")

(let ((class '((class color) (min-colors 88) (background light)))
      (eclipse-dark-bg "#000000")
      (eclipse-dark-fg "#ffffff")
      (eclipse-dark-const "#110099")
      (eclipse-dark-comment "#3F7F5F")
      (eclipse-dark-error "#FF0000")
      (eclipse-dark-builtin "#7F0055")
      (eclipse-dark-string "#2A00FF")
      (eclipse-dark-blue-3 "#758BC6")
      (eclipse-dark-region "#f9b593")
      (eclipse-dark-shadow "grey50"))
  (apply 'custom-theme-set-faces 'eclipse-dark
         (mapcar
          (lambda (x) `(,(car x) ((,class ,(cdr x)))))
          `((default
              :foreground ,eclipse-dark-fg
              :background ,eclipse-dark-bg)
            (cursor :background ,eclipse-dark-fg)
            (shadow :foreground ,eclipse-dark-shadow)
            (success :foreground ,eclipse-dark-error)
            (error :foreground ,eclipse-dark-error :weight bold)
            (warning :foreground "DarkOrange" :weight bold)
            (compilation-warning :underline t :inherit warning)
            (compilation-error :underline t :inherit error)
            (highlight :background "darkseagreen2")
            (fringe :background ,eclipse-dark-bg)
            (region :background ,eclipse-dark-region :foreground ,eclipse-dark-bg)
            (secondary-selection :background "#333366" :foreground "#f6f3e8")
            (whitespace-indentation :background "LightYellow" :foreground "lightgray")
            (term)
            ;; (font-lock-negation-char-face :foreground "#e8e2b7")
            (font-lock-builtin-face :foreground ,eclipse-dark-builtin :bold t)
            (font-lock-comment-face :foreground ,eclipse-dark-comment :slant normal)
            (font-lock-comment-delimiter-face :foreground ,eclipse-dark-comment :slant normal)
            (font-lock-constant-face :foreground ,eclipse-dark-const)
            (font-lock-doc-face :foreground ,eclipse-dark-string)
            (font-lock-doc-string-face :foreground ,eclipse-dark-string)
            (font-lock-function-name-face :foreground ,eclipse-dark-fg :bold t)
            (font-lock-keyword-face :foreground ,eclipse-dark-builtin :weight bold)
            (font-lock-preprocessor-face :foreground ,eclipse-dark-builtin :bold t)
            (font-lock-regexp-grouping-backslash :foreground ,eclipse-dark-builtin)
            (font-lock-regexp-grouping-construct :foreground ,eclipse-dark-builtin)
            (font-lock-string-face :foreground ,eclipse-dark-string)
            (font-lock-type-face :foreground ,eclipse-dark-fg :underline t :slant italic)
            (font-lock-variable-name-face :foreground ,eclipse-dark-fg)
            (font-lock-warning-face :foreground ,eclipse-dark-error)
            (org-code :foreground ,eclipse-dark-builtin :weight bold)
            (org-verbatim :foreground ,eclipse-dark-const)
            (org-level-1 :weight bold :foreground "black")
            (org-level-2 :weight bold :foreground ,eclipse-dark-builtin)
            (org-level-3 :foreground "#123555")
            (org-level-4 :weight bold :slant normal :foreground "#E3258D")
            (org-level-5 :weight bold :slant normal :foreground "#0077CC")
            (org-level-6 :weight bold :slant italic :foreground "#EA6300")
            (org-level-7 :weight bold :slant italic :foreground "#2EAE2C")
            (org-level-8 :weight bold :slant italic :foreground "#FD8008")
            (org-block-begin-line :foreground ,eclipse-dark-const)
            (org-block-end-line :foreground ,eclipse-dark-const)
            (org-scheduled-previously :foreground ,eclipse-dark-comment)
            (ido-subdir :weight bold)
            (mode-line :foreground "black" :background "#f9b593" :box nil)
            (mode-line-inactive :foreground "grey20" :background "grey90" :box nil)
            (minibuffer-prompt :foreground "medium blue")
            (hl-line :background "#e5e4e2")
            ;; defaults
            (mode-line-buffer-id)
            (show-paren-match :background "turquoise")
            (isearch :background "magenta3" :foreground "lightskyblue1")
            (link :foreground "RoyalBlue3" :underline t)
            ;; other packages
            (helm-locate-finish :foreground ,eclipse-dark-const)
            (aw-mode-line-face :foreground ,eclipse-dark-string)
            (swiper-match-face-1 :background "#FEEA89")
            (swiper-match-face-2 :background "#fb7905")
            (swiper-match-face-3 :background "#F9A35A")
            (swiper-match-face-4 :background "#F15C79")
            (swiper-line-face :background "#f3d3d3")
            (hydra-face-red :foreground "#cc0000" :bold t)
            (hydra-face-blue :foreground "RoyalBlue3" :bold t)
            (powerline-active1 :background "grey22" :foreground "white" :inherit mode-line)
            (powerline-active2 :background "grey40" :foreground "white" :inherit mode-line)
            (powerline-inactive1 :background "grey22" :foreground "white" :inherit mode-line-inactive)
            (powerline-inactive2 :background "grey40" :foreground "white" :inherit mode-line-inactive)
            (magit-tag :background "LemonChiffon1" :foreground "goldenrod4")
            (magit-section-heading :inherit header-line)
            (magit-section-highlight :weight bold)
            (magit-diff-context :foreground "grey20")
            (magit-diff-context-highlight :weight bold :foreground "grey20")
            (magit-diff-added :inherit diff-added)
            (magit-diff-added-highlight :inherit diff-added :weight bold)
            (magit-diff-removed :inherit diff-removed)
            (magit-diff-removed-highlight :inherit diff-removed :weight bold)
            (magit-diff-file-heading)
            (magit-diff-file-heading-highlight :weight bold)
            (magit-diff-file-heading-selection :foreground "red")
            (magit-diff-hunk-heading :inherit diff-hunk-header)
            (magit-diff-hunk-heading-highlight :inherit diff-hunk-header :weight bold)
            (magit-hash :foreground "firebrick")
            (magit-branch-remote :background "Grey85" :foreground "OliveDrab4" :box t)
            (magit-branch-local :background "Grey85" :foreground "LightSkyBlue4" :box t)
            (cider-instrumented-face)))))

(custom-theme-set-variables
 'eclipse-dark
 '(ansi-color-names-vector ["#242424" "#e5786d" "#95e454" "#cae682"
                            "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"]))

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide 'eclipse-dark-theme)

;;; eclipse-dark-theme.el ends here
