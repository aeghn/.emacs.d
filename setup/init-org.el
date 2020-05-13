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
(use-package org-superstar
  :config
  (setq org-superstar-headline-bullets-list '("◉" "◎" "☉" "○")
        )
  (add-hook 'org-mode-hook (lambda () (org-superstar-mode 1))))

(setq org-ellipsis " ...")

;; make available "org-bullet-face" such that I can control the font size individually
(setq org-bullets-face-name (quote org-bullet-face))
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
      '(("t" "ToDo" entry (file "~/orgs/chinbox/inbox.org")
         "* TODO %^{ToDo}\n  ADDED: %U " :empty-lines 1 :kill-buffer t)
        ("n" "Note" entry (file "~/orgs/chinbox/inbox.org")
         "* %^{note}\n     %U " :empty-lines 1 :kill-buffer t)))
(global-set-key (kbd "C-c o c") 'org-capture)
(setq org-agenda-files (list "~/orgs/chinbox/inbox.org"))
(global-set-key (kbd "C-c o a") 'org-agenda)

(provide 'init-org)
