(require 's)
(require 'dash)

;; %-Constructs - GNU Emacs Lisp Reference Manual: https://www.gnu.org/software/emacs/manual/html_node/elisp/_0025_002dConstructs.html

;;; Mode line
(defun buffer-status ()
  (if buffer-read-only
      (if (buffer-modified-p)
          (propertize "■")
        (propertize "□")
        )
    (if overwrite-mode
        (if (buffer-modified-p)
            (propertize "▲")
          (propertize "△")
          )
      (if (buffer-modified-p)
          (propertize "●")
        (propertize "◎")))))

(use-package mini-modeline
  :init
  (setq mini-modeline-r-format
        '(("%e"
           (:eval (buffer-status))
           "  %b  "
           "%2l:%2C:%p "
           "  "
           (flycheck-mode flycheck-mode-line)
           "  "
           mode-name
           "  "
           (vc-mode vc-mode)
           mode-line-end-space)))
  (setq mini-modeline-enhance-visual nil
        mini-modeline-echo-duration 2)
  (add-hook 'after-make-frame-functions (lambda (frame)
                                          (select-frame frame)
                                        (mini-modeline-mode t))
                                    ))

(global-set-key (kbd "M-m m f") (lambda ()
                                  (interactive)
                                  (message "Current file is: %s" buffer-file-name)
                                  ))
;;; Frame title format

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
    (:eval (if (buffer-file-name) (concat "  " (shrink-path--dirs-internal default-directory 15)))))
  "Formats the current directory.")
(put 'mode-line-directory 'risky-local-variable t)

(defvar chin/frame-dir
  '(:propertize
    (:eval (if (buffer-file-name) default-directory))))
(put 'chin/frame-dir 'risky-local-variable t)


(setq-default frame-title-format '("%e" (:eval (buffer-status)) " " chin/frame-dir "%b" " - emacs"))

(provide 'init-modeline)
