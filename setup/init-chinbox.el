;; theme
(load-theme 'stein t)

;; modeline
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Mode-Line-Variables.html
;;  idea:
;;  [Change State] [Buffer Name] [Line Number]
(setq mode-line-position
      '(;; %p print percent of buffer above top of window, o Top, Bot or All
        ;; (-3 "%p")
        ;; %I print the size of the buffer, with kmG etc
        ;; (size-indication-mode ("/" (-4 "%I")))
        ;; " "
        ;; %l print the current line number
        ;; %c print the current column
        (line-number-mode ("%l" (column-number-mode ":%c")))))

(setq-default mode-line-format
              '("%e"
                mode-line-front-space
                mode-line-modified
                " %l:%C:%p "                
                mode-line-buffer-identification
                ))

;; keys


;; misc
;; show the outline postion now
(defun org-where-am-i ()
  "Returns a string of headers indicating where point is in the
current tree."
  (interactive)
  (let (headers)
    (save-excursion
      (while (condition-case nil
                 (progn
                   (push (nth 4 (org-heading-components)) headers)
                   (outline-up-heading 1))
               (error nil))))
    (message (mapconcat #'identity headers " > "))))
(global-set-key (kbd "C-x p") 'org-where-am-i)

(provide 'init-chinbox)
