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

(use-package speedbar
  :init
  (setq speedbar-use-images nil)
  )

;; (use-package mini-modeline
  ;; :init
  ;; (setq mini-modeline-r-format
        ;; '(("%e"
           ;; (:eval (buffer-status))
           ;; " "
           ;; mode-line-directory
           ;; "%b  "
           ;; "%2l:%2C:%p "
           ;; "  "
           ;; (flycheck-mode flycheck-mode-line)
           ;; "  "
           ;; mode-name
           ;; "  "
           ;; (vc-mode vc-mode)
           ;; mode-line-end-space)))
  ;; (setq mini-modeline-enhance-visual nil
        ;; mini-modeline-echo-duration 3)
  ;; (mini-modeline-mode t))

;; Thanks to DogLooksGood
;; (setq-default window-divider-default-right-width 1
              ;; window-divider-default-bottom-width 1
              ;; window-divider-default-places t)
;; https://github.com/DogLooksGood/dogEmacs/blob/master/elisp/init-look-and-feel.el#L66
;; (defun chin/toggle-window-divider-and-border ()
  ;; "While there are more then one window in a frame, show the border"
  ;; (unless (string-match-p ".*-posframe\\*" (buffer-name (current-buffer)))
    ;; (if (> (count-windows) 1)
        ;; (progn
          ;; (window-divider-mode 1))
      ;; (progn                            ;
        ;; (window-divider-mode -1)))))

;; (add-hook 'window-configuration-change-hook #'chin/toggle-window-divider-and-border)



(global-set-key (kbd "M-m m f") (lambda ()
                                  (interactive)
                                  (message "Current file is: %s" buffer-file-name)))

;; (setq-default mode-line-format nil)

;; Reference: view-echo-area-messages
(global-set-key (kbd "M-m n") (lambda ()
                                (interactive)
                                (with-current-buffer (messages-buffer)
                                  (goto-char (point-max))
                                  (let ((win (display-buffer (current-buffer))))
                                    (select-window win)))))


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
    (:eval (if (buffer-file-name) (concat "  " (shrink-path--dirs-internal default-directory 30)))))
  "Formats the current directory.")
(put 'mode-line-directory 'risky-local-variable t)

(setq-default frame-title-format '("%e" "%b" " - emacs"))

(provide 'init-interface)
