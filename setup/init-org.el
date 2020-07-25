;;; init-org.el --- Org's settings

;;; Commentary
;; Wang Chung's Emacs config

;; Inspired by https://zzamboni.org/post/beautifying-org-mode-in-emacs/

;;; Code:

;; ;;;;;;;;;;;;;;;;;;;;;;
;;
;; Beautify
;;
;; ;;;;;;;;;;;;;;;;;;;;;;

;; Hide markup
;;(setq org-hide-emphasis-markers t)

;; org-bullets
;; (use-package org-bullets
;;   :config
;;   (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

;; ;; use org-bullets-mode for utf8 symbols as org bullets
;; (require 'org-bullets)
;; (setq org-bullets-face-name (quote org-bullet-face))
;; (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
;; (setq org-bullets-bullet-list '("◉" "◎" "☉" "○"))
;;  "◉"  "▣" "◈" "◬"
;;"㊎" "㊍" "㊌" "㊋" "㊏"

;; org ellipsis options, other than the default Go to Node.
;; not supported in common font, but supported in Symbola (my fall-back font) ⬎,     ⤷, ⤵
;; Use variable width font faces in current buffer

;; Setting fonts for org-mode

(use-package org-superstar
  :config
  (setq org-superstar-headline-bullets-list '("◉" "◎" "☉" "○")
        )
  (add-hook 'org-mode-hook (lambda () (org-superstar-mode 1))))

(use-package valign
  :load-path "/home/chin/.emacs.d/site-lisp/valign")

(setq org-ellipsis " ...")

;; make available "org-bullet-face" such that I can control the font size individually
;; (setq org-bullets-face-name (quote org-bullet-face))
(setq org-log-done 'time)
(setq org-hide-emphasis-markers nil)
(setq org-src-tab-acts-natively t)

;;(add-hook 'org-mode-hook 'visual-line-mode)
(setq org-todo-keywords
      '((sequence "TODO(t)" "STARTED(s)" "WAIT(w)" "|" "DONE(d)" "CANCELED(c)")))

;; change the color of the done headline
(setq org-fontify-whole-heading-line t
      org-fontify-done-headline t
      org-fontify-quote-and-verse-blocks t
      )

;; Open pdf files with pdf-tools
(eval-after-load 'org '(require 'org-pdfview))
(add-to-list 'org-file-apps
             '("\\.pdf\\'" . (lambda (file link)
                               (org-pdfview-open link))))

(require 'org-capture)
(setq org-capture-templates
      '(("t" "ToDo" entry (file (expand-file-name "test.org" default-directory))
         "* TODO %^{ToDo}\n  ADDED: %U " :empty-lines 1 :kill-buffer t)
        ("n" "Note" entry (file "~/orgs/chinbox/inbox.org")
         "* %^{note}\n     %U " :empty-lines 1 :kill-buffer t)))
(global-set-key (kbd "C-c o c") 'org-capture)
(setq org-agenda-files (list "~/orgs/chinbox/inbox.org"))
(global-set-key (kbd "C-c o a") 'org-agenda)


(use-package org-download
  :hook (org-mode-hook . org-download-enable))

(use-package org-roam
  (setq org-roam-directory "~/orgs/org-roam/"))

(use-package org-roam-server
  :after org-roam
  :config
  (setq org-roam-server-host "127.0.0.1"
        org-roam-server-port 8078
        org-roam-server-export-inline-images t
        org-roam-server-authenticate nil
        org-roam-server-label-truncate t
        org-roam-server-label-truncate-length 60
        org-roam-server-label-wrap-length 20)
  (defun org-roam-server-open ()
    "Ensure the server is active, then open the roam graph."
    (interactive)
    (org-roam-server-mode 1)
    (browse-url-xdg-open (format "http://localhost:%d" org-roam-server-port))))


(provide 'init-org)
