(defun goto-match-paren ()
  "Go to the matching parenthesis if on parenthesis, otherwise insert %.
vi style of % jumping to matching brace."
  (interactive)
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
        (t)))

(global-set-key (kbd "C-'") 'goto-match-paren)

(defun indent-current-buffer ()
  "indent whole buffer"
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))

(defcustom chin/indent-sensitive-modes
  '(coffee-mode python-mode slim-mode haml-mode yaml-mode)
  "Modes for which auto-indenting is suppressed."
  :type 'list)

(defun indent-region-or-buffer ()
  "Indent a region if selected, otherwise the whole buffer."
  (interactive)
  (unless (member major-mode chin/indent-sensitive-modes)
    (save-excursion
      (if (region-active-p)
          (progn
            (indent-region (region-beginning) (region-end))
            (untabify (region-beginning) (region-end))
            (message "Indented selected region."))
        (progn
          (indent-current-buffer)
          (message "Indented buffer.")))
      (whitespace-cleanup))))

(global-set-key (kbd "C-c i") 'indent-region-or-buffer)

;; recentf
(use-package recentf
  :ensure nil
  :bind (("C-x C-r" . recentf-open-files))
  :hook (after-init . recentf-mode)
  :init
  (setq recentf-max-saved-items 300)
  :config
  (recentf-track-opened-file  ))

(use-package helpful
  :commands helpful--buffer
  :bind
  (("C-c C-d" . helpful-at-point)
   (:map help-map
         ("k" . helpful-key)
         ("F" . helpful-command)
         ("f" . helpful-callable)
         ("v" . helpful-variable))))

(use-package keyfreq
  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

(global-display-line-numbers-mode 1)

;;; init-default.el ends here
(provide 'init-default)
