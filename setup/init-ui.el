;; Theme settings
(require 'init-reader)
(load-theme 'stein t)

(defconst chin/pdf-view-custom-mode  t)
(ignore-errors
  (chin/pdf-view-mode-setup))

(provide 'init-ui)
