(use-package posframe
  :init (require 'posframe))

;; (use-package pyim
;;   :after liberime
;;   :demand t
;;   :config
;;   (if (display-graphic-p)
;;       (setq pyim-page-tooltip 'posframe)
;;     (setq pyim-page-tooltip 'popup))

;;   (setq default-input-method "pyim")
;;   (setq pyim-default-scheme 'rime)

;;   ;; 设置 pyim 探针设置，这是 pyim 高级功能设置，可以实现 *无痛* 中英文切换 :-)
;;   ;; 我自己使用的中英文动态切换规则是：
;;   ;; 1. 光标只有在注释里面时，才可以输入中文。
;;   ;; 2. 光标前是汉字字符时，才能输入中文。
;;   ;; 3. 使用 M-j 快捷键，强制将光标前的拼音字符串转换为中文。
;;   (setq-default pyim-english-input-switch-functions
;;                 '(pyim-probe-dynamic-english
;;                   pyim-probe-isearch-mode
;;                   pyim-probe-program-mode
;;                   pyim-probe-org-structure-template))

;;   (setq-default pyim-punctuation-half-width-functions
;;                 '(pyim-probe-punctuation-line-beginning
;;                   pyim-probe-punctuation-after-punctuation))

;;   ;; 开启拼音搜索功能
;;   ;;(pyim-isearch-mode 1)

;;   ;; 选词框显示5个候选词
;;   (setq pyim-page-length 5)

;;   :bind
;;   (("M-j" . pyim-convert-string-at-point) ;与 pyim-probe-dynamic-english 配合

;;    :map pyim-mode-map
;;    ("C-h" . pyim-entered-delete-backward-char)
;;    ))

;; (use-package liberime
;;   :init
;;   (setq liberime-user-data-dir (expand-file-name "rime" chin/temporary-files-directory ))
;;   (setq liberime-shared-data-dir (expand-file-name "~/.config/fcitx/rime/"))
;;   (add-hook 'liberime-after-start-hook
;;             (lambda () (liberime-select-schema "double_pinyin_flypy")))
;;   :config
;;   (unless (file-exists-p (concat (liberime-get-library-directory)
;;                                  "build/liberime-core"
;;                                  module-file-suffix))
;;     (liberime-build)))



(use-package rime
  :bind
  (("M-j" . rime-force-enable)
   ("M-m c" . rime-switch-mode))

  :config
  (setq rime-user-data-dir "~/.config/fcitx/rime")

  (setq rime-translate-keybindings
        '("C-f" "C-b" "C-n" "C-p" "C-g"))
  (setq default-input-method "rime"
        rime-show-candidate 'posframe
        rime-show-preedit 'inline)
  (defvar rime-input-mode nil)
  (setq rime-posframe-properties
        (list :font "AMS"
              :internal-border-width 10))
  ;; Use English if return t,
  ;; use Chinese if return nil.
  (defun rime-english-mode ()
    "Start new line with English.
Chinese followed by Chinese.
Space followed by English."
    (if (> (point) (save-excursion (back-to-indentation) (point)))
        (not (looking-back "\\cc" 1))
      t))
  (defun rime-chinese-mode ()
    "Start new line with Chinese.
Chinese followed by Chinese.
English followed by English.
Space followed by English."
    (activate-input-method "rime")
    (cond
     ((org-in-src-block-p) t)
     (chin/window-manager t)
     ((> (point) (save-excursion (back-to-indentation) (point)))
      (if (looking-back " +" 1)
          (looking-back "\\cc +" 2)
        (not (looking-back "\\cc" 1))))))
  
  (defun rime-switch-mode ()
    "Switch between Chinese and English modes."
    (interactive)
    (if (member 'rime-chinese-mode rime-disable-predicates)
        (progn (setq rime-disable-predicates
                     '(rime-english-mode))
               (setq rime-input-mode "English"))
      (progn (setq rime-disable-predicates
                   '(rime-chinese-mode))
             (setq rime-input-mode "Chinese")))
    (if (string= current-input-method "rime")
        (message "Current input method is %s and mode is %s" current-input-method rime-input-mode)
      (activate-input-method 'rime)))

  (setq rime-disable-predicates
        '(rime-english-mode)))

(use-package cal-china-x
  :after calendar
  :config
  (setq calendar-mark-holidays-flag t
        cal-china-x-important-holidays cal-china-x-chinese-holidays
        calendar-holidays (append
                           cal-china-x-important-holidays
                           cal-china-x-general-holidays)))

(provide 'init-chinese)
