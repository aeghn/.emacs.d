;; Theme settings
(require 'init-reader)
(load-theme 'stein t)

(defconst chin/pdf-view-custom-mode  nil)
(ignore-errors
  (chin/pdf-view-mode-setup))

(provide 'init-ui)
