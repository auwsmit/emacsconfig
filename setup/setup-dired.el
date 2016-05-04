;; Dired enable find-alternate-file
;; otherwise Dired likes to create tons of extra buffers
(put 'dired-find-alternate-file 'disabled nil)

;; Human readable filesize
(setq-default dired-listing-switches "-alh")

(provide 'setup-dired)
