;; font settings.
(set-face-attribute 'default nil
                    :family "AMS"
                    :height 120
                    :weight 'normal
                    :width 'normal)

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
(setq package-archives '(("gnu"   . "http://mirrors.163.com/elpa/gnu/")
                         ("melpa" . "http://mirrors.163.com/elpa/melpa/")
                         ("org"   . "http://mirrors.163.com/elpa/org/")))
(package-initialize)

;; (setq use-package-always-ensure t)
;; (setq use-package-always-defer t)
(require 'use-package)

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

(bind-keys :prefix-map chin/plugins-map
           :prefix "M-m")

(bind-key "C-a" 'chin/move-beginning-of-line)
(bind-key "C-h" 'backward-delete-char-untabify)
(bind-key "C-c h" 'help-command)
(bind-key "M-h" 'backward-kill-word)
(bind-key "C-c r" 'chin/revert-buffer)
(bind-key "C-;" 'comment-dwim-2)
(global-set-key (kbd "<f2>") (lambda() (interactive)(find-file "~/.emacs.d/timeline.org")))

(use-package saveplace
  :ensure nil
  :hook (after-init . save-place-mode))

(setq inhibit-compacting-font-caches t)

;;; init-default.el ends here
(provide 'init-basic)
