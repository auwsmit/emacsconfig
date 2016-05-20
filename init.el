;; Disable garbage collection to speed start-up time
;; see: http://tiny.cc/7wd7ay
(let ((gc-cons-threshold most-positive-fixnum))
  (org-babel-load-file "~/.emacs.d/config.org"))
