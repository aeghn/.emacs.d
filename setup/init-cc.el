;; clang-format can be triggered using C-c C-f
;; Create clang-format file using google style
;; clang-format -style=google -dump-config > .clang-format
(use-package cc-mode
  :hook (c-mode-common . (lambda () (c-set-style "stroustrup")))
  :init
  (setq-default c-basic-offset tab-width
                c-backspace-function #'delete-backward-char))

(use-package clang-format
  :bind
  (:map c-mode-map ("C-c C-f" . clang-format-region)))

(use-package ccls
  :hook ((c-mode c++-mode objc-mode) .
         (lambda () (require 'ccls) (lsp))))

(provide 'init-cc)
