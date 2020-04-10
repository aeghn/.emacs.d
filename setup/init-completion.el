;;; init-default.el --- Package's settings

;;; Commentary:

;;; Code:
(use-package yasnippet
  :commands (yas-reload-all)
  :init
  (setq yas-snippet-dirs `(,(expand-file-name "snippets" chin/library-files-directory)))
  (setq yas-wrap-around-region t)
  )

(use-package company
  :defer 2
  :init
  (setq company-tooltip-align-annotations t
        company-idle-delay 0.1
        company-echo-delay 0            ; remove annoying blinking
        company-minimum-prefix-length 3
        company-require-match nil
        company-dabbrev-ignore-case nil
        company-dabbrev-downcase nil)
  (setq company-show-numbers t)
  :bind
  (:map company-active-map
        ("<return>" . newline)
        ("<tab>" . company-complete)
        ("C-t" . company-other-backend)
        ("M-n" . company-select-next)
        ("M-p" . company-select-previous)
        ("RET" . newline)
        ("TAB" . company-select-next))
  :config
  ;; Support yas in commpany
  ;; Note: Must be the last to involve all backends
  (global-company-mode))

(use-package lsp-mode
  :commands lsp
  :init
  :bind (:map lsp-mode-map
              ("C-c C-d" . lsp-describe-thing-at-point)
              ([remap xref-find-definitions] . lsp-find-definition)
              ([remap xref-find-references] . lsp-find-references))
  :config
  (setq lsp-prefer-capf t)
  )

(use-package lsp-ui :commands lsp-ui-mode)
(use-package company-lsp :commands company-lsp)

(provide 'init-completion)
