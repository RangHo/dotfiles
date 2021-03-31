;;; rangho-theme.el --- RangHo's Emacs theme

(deftheme rangho "RangHo's day-to-day Emacs theme")

;; Load values from pywal, if available
(if (file-exists-p (concat user-cache-directory "/wal/colors.el"))
    (require 'pywal-colors (concat user-cache-directory "/wal/colors.el") 'noerror))

;; Disable unnecessary UI elements after loading
(menu-bar-mode 0)
(scroll-bar-mode 0)
(tool-bar-mode 0)

;; Set default font and frame style
(set-face-font 'default "VictorMono Nerd Font-12")
(set-fontset-font t 'hangul "Noto Sans Mono CJK KR-12")
(set-fontset-font t 'kana "Noto Sans Mono CJK JP-12")
(set-fontset-font t 'han "Noto Sans Mono CJK CN-12")
(setq default-frame-alist
      (append (list '(width . 72)
                    '(height . 40)
                    '(internal-border-width . 24)
                    '(font . "VictorMono Nerd Font-12"))))
(set-frame-parameter (selected-frame)
                     'internal-border-width 24)

;; No ugly checkboxes
(setq widget-image-enable nil)

;; Always show matching parentheses
(show-paren-mode t)

;; Enable visual bell
(setq visual-bell t)


(provide-theme 'rangho)
