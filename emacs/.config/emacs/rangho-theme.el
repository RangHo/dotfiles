;;; rangho-theme.el --- RangHo's Emacs theme

;;; Commentary:

;; My Emacs theme.

;;; Code:

(deftheme rangho "RangHo's day-to-day Emacs theme.")


;; ==============
;; User Interface
;; ==============

;; No stupid startup stuff
(setq inhibit-startup-screen t)
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message t)'

;; Set default frame style
(setq default-frame-alist
      (list '(width . 115) ; 80 col of editor + 35 col of treemacs
            '(height . 25)
            '(left-fringe . 0)
            '(right-fringe . 0)
            '(internal-border-width . 10)))

;; Disable unnecessary UI elements after loading
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

;; Disable scrollbars when making new frames
(defun rangho/disable-scroll-bars (frame)
  "Disable scrollbars in FRAME."
  (modify-frame-parameters frame
                           '((vertical-scroll-bars . nil)
                             (horizontal-scroll-bars . nil))))
(add-hook 'after-make-frame-functions 'rangho/disable-scroll-bars)

;; No ugly checkboxes
(setq widget-image-enable nil)

;; Always highlight current line
(global-hl-line-mode 1)

;; Always show matching parentheses
(show-paren-mode 1)

;; Enable visual bell
(setq visual-bell t)


;; ============
;; Font-related
;; ============
;; Emacs has weird way of implementing font lookup, so the function calls below cannot be
;; tampered, or things *will* break.
;;
;; Sample sentences are provided below for reference:
;;   - Alphabet : The quick brown fox jumps over the lazy dog.
;;   - Hangul   : 다람쥐 헌 쳇바퀴에 타고파.
;;   - Kana     : いろはにほへと ちりぬるを / わかよたれそ つねならむ
;;   - Emoji(?) : I'm blue, da💨🎟 ba🐋🐟 dee 🐦👖da📘🔷 ba💙💠 dai🌊🌀
(defun rangho/set-fontset-font ()
  "Set the default font to use throughout Emacs."
  (interactive)
  (set-fontset-font t 'hangul (font-spec :name "semteulche"))
  (set-fontset-font t 'kana (font-spec :name "Noto Sans Mono CJK JP"))
  (set-fontset-font t 'han (font-spec :name "Noto Sans Mono CJK SC"))
  (set-fontset-font t 'han (font-spec :name "Noto Sans Mono CJK TC"))
  (set-fontset-font t 'emoji (font-spec :name "Noto Color Emoji"))

  (create-fontset-from-fontset-spec
   (font-xlfd-name
    (font-spec :name "Noto Serif CJK KR"
               :size 11
               :registry "fontset-variable")))
  (set-fontset-font "fontset-variable" 'hangul (font-spec :name "Noto Serif CJK KR"))
  (set-fontset-font "fontset-variable" 'kana (font-spec :name "Noto Serif CJK JP"))
  (set-fontset-font "fontset-variable" 'han (font-spec :name "Noto Serif CJK SC"))
  (set-fontset-font "fontset-variable" 'han (font-spec :name "Noto Serif CJK TC"))
  (set-fontset-font "fontset-variable" 'emoji (font-spec :name "Noto Color Emoji"))

  (create-fontset-from-fontset-spec
   (font-xlfd-name
    (font-spec :name "semteulche"
               :size 11
               :registry "fontset-fixed")))
  (set-fontset-font "fontset-fixed" 'hangul (font-spec :name "semteulche"))
  (set-fontset-font "fontset-fixed" 'kana (font-spec :name "Noto Sans Mono CJK JP"))
  (set-fontset-font "fontset-fixed" 'han (font-spec :name "Noto Sans Mono CJK SC"))
  (set-fontset-font "fontset-fixed" 'han (font-spec :name "Noto Sans Mono CJK TC"))
  (set-fontset-font "fontset-fixed" 'emoji (font-spec :name "Noto Color Emoji"))

  (set-face-attribute 'default nil
                      :font "semteulche"
                      :height 110)
  (set-face-attribute 'fixed-pitch nil
                      :font "semteulche"
                      :fontset "fontset-fixed"
                      :height 110)
  (set-face-attribute 'variable-pitch nil
                      :font "Noto Serif CJK KR"
                      :fontset "fontset-variable"
                      :height 110))

;; There are three cases to cover:
(cond
 ;; 1. daemon mode --- set the font when a new frame is created
 ((daemonp)
  (add-hook 'after-make-frame-functions
            (lambda (frame)
              (select-frame frame)
              (if (display-graphic-p frame)
                  (rangho/set-fontset-font)))))
 ;; 2. graphical mode --- set the font right away
 ((display-graphic-p)
  (rangho/set-fontset-font))
 ;; 3. terminal mode --- do nothing
 (t nil))

;; Enable font ligatures
;; Emacs 27 has a problem dealing with ligatures.
;; It's not worth trying to workaround this issue as I have to do make significant
;; changes that I'd rather compile Emacs myself.
(unless (= emacs-major-version 27)
  (use-package ligature
    :config
    (ligature-set-ligatures 'prog-mode
                            '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>" ":::" "::="
                              "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!==" "!!." ">=>" ">>="
                              ">>>" ">>-" ">->" "->>" "-->" "---" "-<<" "<~~" "<~>" "<*>" "<||"
                              "<|>" "<$>" "<==" "<=>" "<=<" "<->" "<--" "<-<" "<<=" "<<-" "<<<"
                              "<+>" "</>" "###" "#_(" "..<" "..." "+++" "/==" "///" "_|_" "www"
                              "&&" "^=" "~~" "~@" "~=" "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|="
                              "|>" "|-" "{|" "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!"
                              ">:" ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:" "<$"
                              "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!" "##" "#(" "#?"
                              "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:" "?=" "?." "??" ";;" "/*"
                              "/=" "/>" "//" "__" "~~" "(*" "*)" "\\\\" "://"))
    (global-ligature-mode)))


;; ===========
;; Colorscheme
;; ===========

;; Load values from pywal, if available
;; If colorscheme is not available, use a sensible default
(if (file-exists-p (concat user-cache-directory "wal/colors.el"))
    (require 'pywal-colors (concat user-cache-directory "wal/colors.el") 'noerror)
  (setq-default wal/foreground "#f8f8f2"
                wal/background "#282a36"
                wal/cursor     "#44475a"
                wal/color0     "#21222c"
                wal/color1     "#ff5555"
                wal/color2     "#50fa7b"
                wal/color3     "#f1fa8c"
                wal/color4     "#bd93f9"
                wal/color5     "#ff79c6"
                wal/color6     "#8be9fd"
                wal/color7     "#f8f8f2"
                wal/color8     "#6272a4"
                wal/color9     "#ff6e6e"
                wal/color10    "#69ff94"
                wal/color11    "#ffffa5"
                wal/color12    "#d6acff"
                wal/color13    "#ff92df"
                wal/color14    "#a4ffff"
                wal/color15    "#ffffff"))

;; Map color values to their representative colors
(defvar rangho/color-name-alist
  `((color/foreground      . ,wal/foreground)
    (color/background      . ,wal/background)
    (color/cursor          . ,wal/cursor)
    (color/black           . ,wal/color0)
    (color/intense-black   . ,wal/color8)
    (color/red             . ,wal/color1)
    (color/intense-red     . ,wal/color9)
    (color/green           . ,wal/color2)
    (color/intense-green   . ,wal/color10)
    (color/yellow          . ,wal/color3)
    (color/intense-yellow  . ,wal/color11)
    (color/blue            . ,wal/color4)
    (color/intense-blue    . ,wal/color12)
    (color/magenta         . ,wal/color5)
    (color/intense-magenta . ,wal/color13)
    (color/cyan            . ,wal/color6)
    (color/intense-cyan    . ,wal/color14)
    (color/white           . ,wal/color7)
    (color/intense-white   . ,wal/color15))
  "Association list to map wal colors to friendly names.")

;; Load color utility
(require 'color)

;; List of shades to generate for each colors
(defvar rangho/color-shade-list
  (number-sequence -100 100 5)
  "List of percentage values to lighten/darken the original colors.")

;; Color manipulation utility
(defun color-hex-to-rgb (color)
  "Decompose hex representation of a COLOR to 3-tuple (r, g, b)."
  (list (/ (string-to-number (substring color 1 3) 16) 255.0)
        (/ (string-to-number (substring color 3 5) 16) 255.0)
        (/ (string-to-number (substring color 5) 16) 255.0)))

(defun color-lighten-hex (color percent)
  "Lighten or darken an RGB color string COLOR by PERCENT."
  (let* ((rgb (color-hex-to-rgb color))
         (hsl (apply 'color-rgb-to-hsl rgb))
         (hsl2 (apply 'color-lighten-hsl (nconc hsl `(,percent))))
         (rgb2 (apply 'color-hsl-to-rgb hsl2)))
    (apply 'color-rgb-to-hex (nconc rgb2 '(2)))))

;; Color shade creation utility
(defun rangho/create-color-matrix-alist (color-alist lighten-list)
  "Create a matrix of colors based on a COLOR-ALIST and LIGHTEN-LIST."
  ;; For each color association...
  (append color-alist
          (mapcan (lambda (original-color)
                    ;; And then for each lighten value...
                    (mapcar (lambda (lighten-value)
                              ;; Create a list of...
                              (cons (intern (concat (symbol-name (car original-color))
                                                    (if (>= lighten-value 0) "+" "")
                                                    (number-to-string lighten-value)))
                                    (color-lighten-hex (cdr original-color) lighten-value)))
                            lighten-list))
                  color-alist)))

;; Create a color matrix
(defvar rangho/color-matrix-alist
  (rangho/create-color-matrix-alist rangho/color-name-alist
                                    rangho/color-shade-list))

;; Apply the color theme
(require 'let-alist)
(let-alist rangho/color-matrix-alist
  (custom-theme-set-faces
   'rangho

   ;; Basic UI elements
   `(border              ((t (:foreground ,.color/foreground+10))))
   `(button              ((t (:underline t))))
   `(cursor              ((t (:foreground ,.color/background :background ,.color/foreground))))
   `(default             ((t (:foreground ,.color/foreground :background ,.color/background))))
   `(default-italic      ((t (:slant italic))))
   `(error               ((t (:foreground ,.color/red :weight bold))))
   `(ffap                ((t (:foreground ,.color/foreground-20))))
   `(fringe              ((t (:background ,.color/background-10))))
   `(header-line         ((t (:inherit mode-line))))
   `(highlight           ((t (:foreground ,.color/intense-white :background ,.color/intense-black))))
   `(hl-line             ((t (:background ,.color/cursor :extend t))))
   `(info-quoted-name    ((t (:foreground ,.color/intense-red))))
   `(info-string         ((t (:foreground ,.color/intense-yellow))))
   `(line-number         ((t (:foreground ,.color/intense-black :slant italic))))
   `(link                ((t (:foreground ,.color/cyan :underline t :weight bold))))
   `(link-visited        ((t (:foreground ,.color/blue :underline t :weight normal))))
   `(match               ((t (:foreground ,.color/background :background ,.color/yellow))))
   `(menu                ((t (:background ,.color/current :inverse-video nil))))
   `(minibuffer-prompt   ((t (:foreground ,.color/intense-magenta :weight bold))))
   `(mode-line           ((t (:foreground
                              ,.color/background-20
                              :background
                              ,.color/foreground+20
                              :box
                              (:line-width 3 :color ,.color/foreground+10 :style nil)))))
   `(mode-line-inactive  ((t (:foreground
                              ,.color/foreground-20
                              :background
                              ,.color/background+20
                              :box
                              (:line-width 3 :color ,.color/background+10 :style nil)))))
   `(mode-line-highlight ((t (:inherit highlight))))
   `(mode-line-emphasis  ((t (:weight regular))))
   `(mode-line-buffer-id ((t (:weight regular))))
   `(region              ((t (:inherit highlight))))
   `(shadow              ((t (:foreground ,.color/intense-black))))
   `(success             ((t (:foreground ,.color/green :weight bold))))
   `(tooltip             ((t (:foregroud ,.color/foreground :background ,.color/cursor))))
   `(trailing-whitespace ((t (:background ,.color/intense-yellow))))
   `(vertical-border     ((t (:foreground ,.color/foreground-10))))
   `(warning             ((t (:foreground ,.color/yellow :weight bold))))
   
   ;; font-lock!
   `(font-lock-builtin-face           ((t (:foreground ,.color/cyan :slant italic))))
   `(font-lock-comment-face           ((t (:foreground ,.color/intense-black))))
   `(font-lock-comment-delimiter-face ((t (:inherit font-lock-comment-face))))
   `(font-lock-constant-face          ((t (:foreground ,.color/intense-magenta))))
   `(font-lock-doc-face               ((t (:foreground ,.color/intense-black+20))))
   `(font-lock-function-name-face     ((t (:foreground ,.color/green))))
   `(font-lock-keyword-face           ((t (:foreground ,.color/magenta))))
   `(font-lock-negation-char-face     ((t (:foreground ,.color/cyan))))
   `(font-lock-number-face            ((t (:inherit font-lock-constant-face))))
   `(font-lock-operator-face          ((t (:inherit font-lock-keyword-face))))
   `(font-lock-preprocessor-face      ((t (:foreground ,.color/intense-red))))
   `(font-lock-string-face            ((t (:foreground ,.color/intense-yellow))))
   `(font-lock-type-face              ((t (:inherit font-lock-builtin-face))))
   `(font-lock-variable-name-face     ((t (:foreground ,.color/yellow))))
   `(font-lock-warning-face           ((t (:inherit warining))))

   ;; ANSI colors
   `(ansi-color-black          ((t (:foreground ,.color/black :background ,.color/black))))
   `(ansi-color-blue           ((t (:foreground ,.color/blue :background ,.color/blue))))
   `(ansi-color-cyan           ((t (:foreground ,.color/cyan :background ,.color/cyan))))
   `(ansi-color-green          ((t (:foreground ,.color/green :background ,.color/green))))
   `(ansi-color-magenta        ((t (:foreground ,.color/magenta :background ,.color/magenta))))
   `(ansi-color-red            ((t (:foreground ,.color/red :background ,.color/red))))
   `(ansi-color-white          ((t (:foreground ,.color/white :background ,.color/white))))
   `(ansi-color-yellow         ((t (:foreground ,.color/yellow :background ,.color/yellow))))
   `(ansi-color-bright-black   ((t (:foreground ,.color/intense-black :background ,.color/intense-black))))
   `(ansi-color-bright-blue    ((t (:foreground ,.color/intense-blue :background ,.color/intense-blue))))
   `(ansi-color-bright-cyan    ((t (:foreground ,.color/intense-cyan :background ,.color/intense-cyan))))
   `(ansi-color-bright-green   ((t (:foreground ,.color/intense-green :background ,.color/intense-green))))
   `(ansi-color-bright-magenta ((t (:foreground ,.color/intense-magenta :background ,.color/intense-magenta))))
   `(ansi-color-bright-red     ((t (:foreground ,.color/intense-red :background ,.color/intense-red))))
   `(ansi-color-bright-white   ((t (:foreground ,.color/intense-white :background ,.color/intense-white))))
   `(ansi-color-bright-yellow  ((t (:foreground ,.color/intense-yellow :background ,.color/intense-yellow))))

   ;; Company
   `(company-echo-common ((t (:foreground ,.color/background :background ,.color/foreground))))

   ;; Diff
   `(diff-added             ((t (:foreground ,.color/foreground :background ,.color/green-40 :extend t))))
   `(diff-removed           ((t (:foreground ,.color/foreground :background ,.color/red-40 :extend t))))
   `(diff-refine-added      ((t (:foreground ,.color/background :background ,.color/green))))
   `(diff-refine-removed    ((t (:foreground ,.color/background :background ,.color/red))))
   `(diff-indicator-added   ((t (:foreground ,.color/green))))
   `(diff-indicator-removed ((t (:foreground ,.color/red))))
   `(diff-indicator-changed ((t (:foreground ,.color/yellow))))
   `(diff-error             ((t (:foreground ,.color/red :background ,.color/background :weight bold))))

   ;; Org
   `(org-block                 ((t (:background ,.color/background+40))))
   `(org-code                  ((t (:foreground ,.color/intense-green))))
   `(org-document-info         ((t (:foreground ,.color/intense-blue))))
   `(org-document-info-keyword ((t (:foreground ,.color/intense-black))))
   `(org-document-title        ((t (:foreground ,.color/intense-red :weight bold :height 1.44))))
   `(org-ellipsis              ((t (:foreground ,.color/intense-black))))
   `(org-footnote              ((t (:foreground ,.color/intense-blue))))
   `(org-formula               ((t (:foreground ,.color/intense-magenta))))
   `(org-level-1               ((t (:foreground ,.color/foreground-40 :weight bold :height 1.3))))
   `(org-level-2               ((t (:foreground ,.color/foreground-20 :weight bold :height 1.1))))
   `(org-level-3               ((t (:foreground ,.color/foreground10  :weight bold :height 1.0))))
   `(org-level-4               ((t (:foreground ,.color/foreground-5 :weight bold :height 1.0))))
   `(org-level-5               ((t (:foreground ,.color/foreground :weight bold :height 1.0))))
   `(org-level-6               ((t (:foreground ,.color/foreground :height 1.0))))
   `(org-level-7               ((t (:foreground ,.color/foreground :height 1.0))))
   `(org-level-8               ((t (:foreground ,.color/foreground :height 1.0))))
   `(org-link                  ((t (:inherit link))))
   `(org-meta-line             ((t (:inherit (font-lock-comment-face fixed-pitch)))))
   `(org-table                 ((t ()))))
   `(org-verbatim              ((t (:foreground ,.color/green)))
   `(org-warning               ((t (:foreground ,.color/yellow :weight bold))))

   ;; Outline
   `(outline-1 ((t (:foreground ,.color/foreground-40 :weight bold :height 1.3))))
   `(outline-2 ((t (:foreground ,.color/foreground-20 :weight bold :height 1.1))))
   `(outline-3 ((t (:foreground ,.color/foreground10  :weight bold :height 1.0))))
   `(outline-4 ((t (:foreground ,.color/foreground-5 :weight bold :height 1.0))))
   `(outline-5 ((t (:foreground ,.color/foreground :weight bold :height 1.0))))
   `(outline-6 ((t (:foreground ,.color/foreground :height 1.0))))
   `(outline-7 ((t (:foreground ,.color/foreground :height 1.0))))
   `(outline-8 ((t (:foreground ,.color/foreground :height 1.0))))
   ))

(provide-theme 'rangho)

;;; rangho-theme.el ends here
