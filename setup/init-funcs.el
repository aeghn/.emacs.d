(defvar work-home (file-truename "~/work/"))

(defun chin/setup-new-script ()
  (interactive)
  (let* ((name-prefix (read-string "Name prefix: "))
         (file-name (concat name-prefix (format-time-string "_%Y%m%d_%H%M.sh")))
         (file-path (expand-file-name file-name work-home)))

    (write-region "#!/usr/bin/env bash\n\n#" nil file-path)
    (shell-command (concat "chmod +x " file-path))
    (find-file file-path)))

(provide init-funcs)
