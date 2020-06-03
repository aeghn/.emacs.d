(require 'package)
(require 'counsel)

(use-package counsel
  :hook ((after-init . ivy-mode)
         (ivy-mode   . counsel-mode))
  :bind (("C-s"   . swiper-isearch)
         ("C-r"   . swiper-isearch-backward)
         ("C-S-f"   . swiper)
         ("C-S-s" . swiper-all)

         :map counsel-mode-map
         ([remap swiper] . counsel-grep-or-swiper)
         ([remap swiper-backward] . counsel-grep-or-swiper-backward)
         ([remap dired] . counsel-dired)
         ([remap set-variable] . counsel-set-variable)
         ([remap insert-char] . counsel-unicode-char)
         ([remap recentf-open-files] . counsel-recentf)
         ("C-c c r" . counsel-rg-with-prompt)
         ("C-c c f" . counsel-fzf-with-prompt)

         :map counsel-find-file-map
         ("C-u" . counsel-up-directory)

         :map swiper-map
         ("M-s" . swiper-isearch-toggle)
         ("M-%" . swiper-query-replace)

         :map isearch-mode-map
         ("M-s" . swiper-isearch-toggle)

         :map ivy-minibuffer-map
         ("M-j" . pyim-convert-string-at-point)
         )

  :init (setq enable-recursive-minibuffers t) ; Allow commands in minibuffers

  (setq ivy-use-selectable-prompt t
        ivy-use-virtual-buffers t    ; Enable bookmarks and recentf
        ivy-height 10
        ivy-fixed-height-minibuffer t
        ivy-count-format "(%d/%d) "
        ivy-on-del-error-function nil
        ivy-initial-inputs-alist nil
        ;; Show full history file name
        ivy-virtual-abbreviate 'abbreviate)
        ;; )

  (setq swiper-action-recenter t)

  (use-package amx
    :init (setq amx-history-length 20)
    )
  :config
  (defun counsel-rg-with-prompt ()
    "A wrapper for counsel-rg which shows search dir."
    (interactive)
    (let ((initial-directory default-directory))
      (when current-prefix-arg
        (setq initial-directory
              (counsel-read-directory-name " rg in directory: ")))
      (counsel-rg nil initial-directory nil (concat "Search " initial-directory " for: "))))

  (defun counsel-fzf-with-prompt ()
    "A wrapper for counsel-fzf which shows search dir."
    (interactive)
    (let ((fzf-basename (car (split-string counsel-fzf-cmd)))
          (initial-directory default-directory))
      (when current-prefix-arg
        (setq initial-directory
              (counsel-read-directory-name (concat
                                            fzf-basename
                                            " in directory: "))))
      (counsel-fzf nil initial-directory (concat "Search " initial-directory " for: ")))))

(use-package ivy-rich
  :init
  (ivy-rich-mode 1)
  ;; For better performance
  (setq ivy-rich-parse-remote-buffer nil))


;;; init-keys.el ends here
(provide 'init-ivy)
