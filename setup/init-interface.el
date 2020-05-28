(require 's)
(require 'dash)

(setq scroll-bar-mode nil
      scroll-conservatively 101
      scroll-margin 4)

;; Mode-line
(use-package minions
  :config
  (add-hook #'after-init-hook #'minions-mode))

(defun buffer-status ()
  "Return different icon for different buffer status"
  (if buffer-read-only (if (buffer-modified-p) "■" "□")
    (if overwrite-mode (if (buffer-modified-p) "▲" "△")
      (if (buffer-modified-p) "●" "◎"))))

;; buffer directory
;; thanks to https://github.com/justinwoo/broken-emacs-packages/blob/master/shrink-path-20170813.247/shrink-path.el
(defun shrink-path--truncate (str)
  "Return STR's first character or first two characters if hidden."
  (substring str 0 (if (s-starts-with? "." str) 2 1)))

(defun shrink-path--dirs-internal (full-path &optional given-length)
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
                              (shrink-path--truncate it)))
             (s-join "/")
             (setq shrunk))
        (s-concat (unless (s-matches? (rx bos (or "~" "/")) shrunk) "/")
                  shrunk
                  (unless (s-ends-with? "/" shrunk) "/")))
    full-path))

(defvar mode-line-directory
  '(:propertize
    (:eval (if (buffer-file-name) (shrink-path--dirs-internal default-directory 15))))
  "Formats the current directory.")
(put 'mode-line-directory 'risky-local-variable t)

;; %-Constructs - GNU Emacs Lisp Reference Manual: https://www.gnu.org/software/emacs/manual/html_node/elisp/_0025_002dConstructs.html
(setq-default mode-line-format
              (let ((spaces "   "))
                `(
                  " "
                  (:eval (buffer-status))
                  ,spaces
                  mode-line-directory
                  "%b"
                  ,spaces
                  "%C %p"
                  ,spaces
                  mode-line-modes
                  vc-mode
                  ,spaces
                  mode-line-misc-info

                  mode-line-end-spaces)))

;; bindings related to mode-line
(global-set-key (kbd "M-m m f") (lambda ()
                                  (interactive)
                                  (message "Current file is: %s" buffer-file-name)))

;; Reference: view-echo-area-messages
(global-set-key (kbd "M-m n") (lambda ()
                                (interactive)
                                (with-current-buffer (messages-buffer)
                                  (goto-char (point-max))
                                  (let ((win (display-buffer (current-buffer))))
                                    (select-window win)))))

(setq-default frame-title-format '("%e" "%b" " - emacs"))

(provide 'init-interface)
