;;; meow-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "meow-command" "meow-command.el" (0 0 0 0))
;;; Generated autoloads from meow-command.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "meow-command" '("meow-")))

;;;***

;;;### (autoloads nil "meow-company" "meow-company.el" (0 0 0 0))
;;; Generated autoloads from meow-company.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "meow-company" '("meow--company-")))

;;;***

;;;### (autoloads nil "meow-core" "meow-core.el" (0 0 0 0))
;;; Generated autoloads from meow-core.el

(autoload 'meow-insert-mode "meow-core" "\
Meow Insert state.

If called interactively, enable Meow-Insert mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(autoload 'meow-normal-mode "meow-core" "\
Meow Normal state.

If called interactively, enable Meow-Normal mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(autoload 'meow-keypad-mode "meow-core" "\
Meow keypad state.

If called interactively, enable Meow-Keypad mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(autoload 'meow-motion-mode "meow-core" "\
Meow motion state.

If called interactively, enable Meow-Motion mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(autoload 'meow-mode "meow-core" "\
Meow minor mode.

If called interactively, enable Meow mode if ARG is positive, and
disable it if ARG is zero or negative.  If called from Lisp, also
enable the mode if ARG is omitted or nil, and toggle it if ARG is
`toggle'; disable the mode otherwise.

This minor mode is used by meow-global-mode, should not be enabled directly.

\(fn &optional ARG)" t nil)

(put 'meow-global-mode 'globalized-minor-mode t)

(defvar meow-global-mode nil "\
Non-nil if Meow-Global mode is enabled.
See the `meow-global-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `meow-global-mode'.")

(custom-autoload 'meow-global-mode "meow-core" nil)

(autoload 'meow-global-mode "meow-core" "\
Toggle Meow mode in all buffers.
With prefix ARG, enable Meow-Global mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Meow mode is enabled in all buffers where
`(lambda nil (unless (minibufferp) (meow-mode 1)))' would do it.
See `meow-mode' for more information on Meow mode.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "meow-core" '("meow-")))

;;;***

;;;### (autoloads nil "meow-eldoc" "meow-eldoc.el" (0 0 0 0))
;;; Generated autoloads from meow-eldoc.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "meow-eldoc" '("meow--eldoc-")))

;;;***

;;;### (autoloads nil "meow-helpers" "meow-helpers.el" (0 0 0 0))
;;; Generated autoloads from meow-helpers.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "meow-helpers" '("meow-")))

;;;***

;;;### (autoloads nil "meow-keymap" "meow-keymap.el" (0 0 0 0))
;;; Generated autoloads from meow-keymap.el

(defvar meow-motion-state-keymap (let ((keymap (make-sparse-keymap))) (define-key keymap [escape] 'meow-last-buffer) keymap) "\
Keymap for Meow motion state.")

(defvar meow-keypad-state-keymap (let ((map (make-sparse-keymap))) (suppress-keymap map t) (define-key map [remap self-insert-command] 'meow-keypad-self-insert) (let ((i 32)) (while (< i 256) (define-key map (vector i) 'meow-keypad-self-insert) (setq i (1+ i))) (define-key map (kbd "DEL") 'meow-keypad-undo) (define-key map (kbd "<backspace>") 'meow-keypad-undo) (define-key map (kbd "<escape>") 'meow-escape-or-normal-modal) (define-key map (kbd "<tab>") 'meow-keypad-self-insert) (define-key map (kbd "TAB") 'meow-keypad-self-insert) (define-key map (kbd "<return>") 'meow-keypad-self-insert) (define-key map (kbd "RET") 'meow-keypad-self-insert)) map) "\
Keymap for Meow keypad state.")

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "meow-keymap" '("meow-")))

;;;***

;;;### (autoloads nil "meow-keypad" "meow-keypad.el" (0 0 0 0))
;;; Generated autoloads from meow-keypad.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "meow-keypad" '("meow-")))

;;;***

;;;### (autoloads nil "meow-tut" "meow-tut.el" (0 0 0 0))
;;; Generated autoloads from meow-tut.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "meow-tut" '("meow-")))

;;;***

;;;### (autoloads nil "meow-util" "meow-util.el" (0 0 0 0))
;;; Generated autoloads from meow-util.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "meow-util" '("meow-")))

;;;***

;;;### (autoloads nil "meow-var" "meow-var.el" (0 0 0 0))
;;; Generated autoloads from meow-var.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "meow-var" '("meow-")))

;;;***

;;;### (autoloads nil "meow-wgrep" "meow-wgrep.el" (0 0 0 0))
;;; Generated autoloads from meow-wgrep.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "meow-wgrep" '("meow--wgrep-")))

;;;***

;;;### (autoloads nil "meow-yas" "meow-yas.el" (0 0 0 0))
;;; Generated autoloads from meow-yas.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "meow-yas" '("meow--yas-setup")))

;;;***

;;;### (autoloads nil nil ("meow-face.el" "meow-pkg.el" "meow.el")
;;;;;;  (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; meow-autoloads.el ends here
