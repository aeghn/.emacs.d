(prefer-coding-system 'utf-8)

(defvar user-emacs-directory (file-truename "~/.emacs.d/"))

(defvar chin/configuration-files-directory (expand-file-name "setup" user-emacs-directory)
  "Personal configuration directory")

(defvar chin/library-files-directory (expand-file-name "library" user-emacs-directory)
  "Personal library directory")

(defvar chin/temporary-files-directory (expand-file-name "litter" user-emacs-directory)
  "Personal temporary directory")
(unless (file-directory-p chin/temporary-files-directory) (mkdir chin/temporary-files-directory))

(unless (file-directory-p chin/temporary-files-directory) (mkdir chin/temporary-files-directory))

(setq custom-file (expand-file-name "custom.el" chin/temporary-files-directory))
(when (file-exists-p custom-file) (load custom-file :no-error :no-message))

(add-to-list 'load-path (expand-file-name chin/configuration-files-directory))
(add-to-list 'custom-theme-load-path (expand-file-name "themes" chin/library-files-directory))

(setq gc-cons-threshold (* 100 1024 1024))

(require 'cl-lib)
(require 'init-basic)

;; (require 'exwm)
;; (require 'exwm-config)
;; (exwm-config-default)

(require 'benchmark-init)
;; To disable collection of benchmark data after init is done.
(add-hook 'after-init-hook 'benchmark-init/deactivate)
;; Choose different load mode
(defvar chin/emacs-load-mode (getenv "_EMACS_LOAD_MODE_"))
(cond ((not chin/emacs-load-mode)
       (progn
         (require 'init-default)
         (require 'init-chinese)
         (require 'init-completion)
         (require 'init-latex)
         (require 'init-ivy)
         (require 'init-org)
         (require 'init-window)
         (require 'init-blog)
         (require 'init-edit)
         (require 'init-reader)
         (require 'init-network)
         (require 'init-ui)
         (require 'init-cc)
         (require 'init-dired)
         (require 'init-interface)))
      ((string-equal "chinbox" chin/emacs-load-mode)
       (progn
         (require 'init-org)
         (require 'init-ivy)
         (require 'init-chinese)
         (require 'init-chinbox))))
