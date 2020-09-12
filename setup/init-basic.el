;; font settings.
(setq inhibit-compacting-font-caches t)


;; Backup file and auto save default files
(setq-default make-backup-files nil
              history-length 1000)

(setq auto-save-default nil
      save-silently t
      auto-save-list-file-prefix (expand-file-name
                                  "auto-save-list/.save-"
                                  chin/temporary-files-directory))

(setq browse-url-browser-function 'browse-url-default-browser)

;; Set tab width
(setq-default tab-width 4)
;; use space to indent by default
(setq-default indent-tabs-mode nil)

;; Change all yes-or-no to y-or-n
(fset 'yes-or-no-p 'y-or-n-p)

(require 'package)
(setq package-archives '(("gnu"   . "https://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                         ("melpa" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
                         ("org"   . "https://mirrors.tuna.tsinghua.edu.cn/elpa/org/")))

(package-initialize)

;; (setq use-package-always-ensure t)
;; (setq use-package-always-defer t)
(require 'use-package)
(setq quelpa-update-melpa-p nil)
(require 'quelpa-use-package)

(use-package no-littering
  :demand t
  :init
  (setq no-littering-etc-directory chin/temporary-files-directory)
  (setq no-littering-var-directory chin/temporary-files-directory))

(defun chin/move-beginning-of-line ()
  "Move point back to indentation of beginning of line or beginning of line."
  (interactive)
  (let ((orig-begin (point)))
    (back-to-indentation)
    (if (= orig-begin (point))
        (beginning-of-line))))

(defun server-shutdown ()
  "Save buffers, quit, and shutdown (kill) server."
  (interactive)
  (save-some-buffers)
  (kill-emacs))

(defun chin/revert-buffer ()
  "Revert buffer without confirming."
  (interactive)
  (revert-buffer t t t)
  (message "buffer is reverted"))

(defun my-insert-tab-char ()
  "Insert a tab char. (ASCII 9, \t)"
  (interactive)
  (insert "\t"))

(global-unset-key (kbd "M-m"))
(define-prefix-command 'chin/private-map)
(global-set-key (kbd "M-m") 'chin/private-map)

(bind-key "C-a" 'chin/move-beginning-of-line)
(bind-key "C-h" 'backward-delete-char-untabify)
(bind-key "C-c h" 'help-command)
(bind-key "M-h" 'backward-kill-word)
(bind-key "C-c r" 'chin/revert-buffer)
(bind-key "C-;" 'comment-dwim-2)
(global-unset-key (kbd "C-z"))
(global-set-key (kbd "<f2>") (lambda() (interactive)(find-file "~/.emacs.d/timeline.org")))


(use-package saveplace
  :ensure nil
  :hook (after-init . save-place-mode))

(setq-default chin/window-manager nil)

(use-package yasnippet
  :diminish yas-minor-mode
  :init
  (add-to-list 'yas-snippet-dirs (expand-file-name "snippets" chin/library-files-directory))
  :hook
  (after-init . yas-global-mode)
  :config
  (require 'warnings)
  (add-to-list 'warning-suppress-types '(yasnippet backquote-change))
  )

;; (defun replace-before-paste ()
;;   (interactive)
;;   (let* ((content (current-kill 0))
;;          (content (s-replace "\"" "\\\"" content))
;;          (content (s-replace "<" "\\<" content))
;;          (content (s-replace ">" "\\>" content)))
;;     (insert content)))




;; (bind-key "C-y" 'replace-before-paste)

;;; init-default.el ends here
(provide 'init-basic)
