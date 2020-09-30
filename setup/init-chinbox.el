;; theme
(load-theme 'stein t)

;; shortcuts
;; Open personal todo list
(bind-key "M-m a" (lambda ()
                    (interactive)
                    (find-file "~/Documents/orgs/chinbox/inbox.org")))
(bind-key "M-m s" (lambda ()
                    (interactive)
                    (find-file (concat "~/Workspace/docs/orgs/now.org"))))

(bind-key "M-m d" (lambda ()
                    (interactive)
                    (find-file (concat "~/Workspace/docs/orgs/"))))


(setq org-agenda-files (directory-files-recursively "~/Workspace/docs/orgs/" "\\.org$"))
(global-set-key (kbd "M-m t") 'org-agenda)

(provide 'init-chinbox)
