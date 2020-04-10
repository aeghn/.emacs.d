(use-package elfeed                     ;
  :hook (elfeed-show-mode . chin/elfeed-show-setup)
  :preface
  (defvar chin/elfeed-org-file "~/orgs/rss.org")
  :bind (("M-m e" . elfeed)
         :map elfeed-search-mode-map
         ("g" . (lambda ()
                  (interactive)
                  (elfeed-org)
                  (elfeed-update)))
         ("f" . (lambda ()
                  (interactive)
                  (find-file chin/elfeed-org-file)))
         :map elfeed-show-mode-map
         ("o" . ace-link)
         ("q" . delete-window))
  :init (setq url-queue-timeout 30
              elfeed-db-directory (expand-file-name "elfeed-db" chin/temporary-files-directory)
              elfeed-show-entry-switch #'pop-to-buffer
              elfeed-show-entry-delete #'delete-window)
  :config
  (use-package elfeed-org
  :config
  (elfeed-org)
  (setq rmh-elfeed-org-files (list chin/elfeed-org-file)))

  (defun chin/elfeed-show-setup ()
    (olivetti-mode 1))

  (defun toggle-proxy-all-proxy()
    "Set curl proxy"
    (interactive)
    (if (getenv "ALL_PROXY")
        (setenv "ALL_PROXY" nil)
      (setenv "ALL_PROXY" "socks5h://127.0.0.1:1080"))
    (message "ALL_PROXY is %s" (getenv "ALL_PROXY"))))

(use-package pdf-view
  :magic ("%PDF" . pdf-view-mode)
  :config
  (pdf-tools-install :no-query)

  (defun chin/pdf-view-switch-current-buffers-theme (theme-mode)
    "Set pdf-view midnight theme as color theme."
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (eq major-mode 'pdf-view-mode)
          (pdf-view-midnight-minor-mode theme-mode)))))

  (defun chin/pdf-view-mode-setup ()
    (interactive)
    (ignore-errors
      (if chin/pdf-view-custom-mode
          (progn
            (chin/pdf-view-switch-current-buffers-theme 1)
            (add-hook 'pdf-tools-enabled-hook 'pdf-view-midnight-minor-mode))
        (progn
          (chin/pdf-view-switch-current-buffers-theme -1)
          (remove-hook 'pdf-tools-enabled-hook 'pdf-view-midnight-minor-mode))
        )))
  (chin/pdf-view-mode-setup))

(use-package nov
  :mode ("\\.epub\\'" . nov-mode)
  :hook (nov-mode . chin/nov-setup)
  :init
  (defun chin/nov-setup ()
    (visual-line-mode 1)
    (olivetti-mode 1)))

(provide 'init-reader)
