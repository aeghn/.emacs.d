(require 's)
(require 'dash)
(require 'minions)

(setq scroll-bar-mode nil
      scroll-conservatively 101
      scroll-margin 4)


;;; Mode-line parts
(defface mode-line-position-face
  '((t :weight normal))
  "Face used for buffer identification parts of the mode line."
  :group 'mode-line-faces
  :group 'basic-faces)

(defvar mode-line-buffer-directory
  '(:propertize
    (:eval (if (buffer-file-name) (chin/shrink-path default-directory 15))))
  "Formats the current directory.")
(put 'mode-line-buffer-directory 'risky-local-variable t)

(defun buffer-status ()
  (if buffer-read-only (if (buffer-modified-p) " Δ " " δ ")
    (if overwrite-mode (if (buffer-modified-p) " Ω " " ω ")
      (if (buffer-modified-p) " Φ " " φ "))))

(defun chin/shrink-path (full-path given-length)
  "Return fish-style truncated string based on FULL-PATH.
Optional parameter TRUNCATE-ALL will cause the function to truncate the last
directory too."
  (if (> (length full-path) given-length)
      (let* ((home (expand-file-name "~"))
             (path (replace-regexp-in-string
                    (s-concat "^" home) "~" full-path))
             (split (s-split "/" path 'omit-nulls))
             (split-len (length split))
             shrunk)
        (->> split
             (--map-indexed (if (= it-index (1- split-len))
                                it
                              (substring it 0 (if (s-starts-with? "." it) 2 1))))
             (s-join "/")
             (setq shrunk))
        (s-concat (unless (s-matches? (rx bos (or "~" "/")) shrunk) "/")
                  shrunk
                  (unless (s-ends-with? "/" shrunk) "/")))
    full-path))


;; bindings related to mode-line
(global-set-key (kbd "M-m m f")
                (lambda ()
                  (interactive)
                  (message "Current file is: %s" buffer-file-name)))

;; Reference: view-echo-area-messages
(global-set-key (kbd "M-m n")
                (lambda ()
                  (interactive)
                  (with-current-buffer (messages-buffer)
                    (goto-char (point-max))
                    (let ((win (display-buffer (current-buffer))))
                      (select-window win)))))

;; %-Constructs - GNU Emacs Lisp Reference Manual: https://www.gnu.org/software/emacs/manual/html_node/elisp/_0025_002dConstructs.html
(setq-default mode-line-format
              `(
                (:eval (buffer-status))
                " "
                mode-line-buffer-directory
                (:eval (propertize "%b" 'face 'mode-line-buffer-id))
                "  "
                (:eval (propertize "%l:%C %p" 'face 'mode-line-position-face))
                "  "
                mode-line-modes
                "  "
                (flycheck-mode flycheck-mode-line)
                "  "
                vc-mode
                "  "
                mode-line-misc-info
                mode-line-end-spaces))

(use-package minions
  :config
  (add-hook #'after-init-hook #'minions-mode))


;;; Header
(setq-default frame-title-format '("%e" "%b" " - emacs"))

(provide 'init-interface)
