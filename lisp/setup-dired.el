;; Dired enable find-alternate-file
;; otherwise Dired likes to create tons of extra buffers
(put 'dired-find-alternate-file 'disabled nil)

(provide 'setup-dired)
