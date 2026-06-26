;;; material-purple-theme.el --- Purple-colored Emacs colorscheme

;;; Commentary:

;;; Code:

(require 'rangho-themes)

(deftheme material-purple "Purple-colored Material Design theme.")

(rangho-themes-set-faces
 'material-purple
 '((primary                   . "#e4b5ff")
   (on-primary                . "#4e0078")
   (primary-container         . "#c267fc")
   (on-primary-container      . "#3a005a")
   (secondary                 . "#764d90")
   (on-secondary              . "#ffffff")
   (secondary-container       . "#e2b2fc")
   (on-secondary-container    . "#684081")
   (tertiary                  . "#ac1c6a")
   (on-tertiary               . "#ffffff")
   (tertiary-container        . "#cd3a83")
   (on-tertiary-container     . "#fffbff")
   (error                     . "#ffb4ab")
   (on-error                  . "#690005")
   (error-container           . "#93000a")
   (on-error-container        . "#ffdad6")
   (surface                   . "#17111a")
   (surface-container-lowest  . "#120c15")
   (surface-container-low     . "#1f1922")
   (surface-container         . "#241d26")
   (surface-container-high    . "#2e2831")
   (surface-container-highest . "#39323c")
   (on-surface                . "#ebdfec")
   (on-surface-variant        . "#d1c2d5")
   (outline                   . "#9a8c9e")
   (outline-variant           . "#4e4352")
   (inverse-surface           . "#ebdfec")
   (inverse-on-surface        . "#352e38")
   (inverse-primary           . "#8a2cc4")
   (scrim                     . "#000000")
   (shadow                    . "#000000")))

(provide-theme 'material-purple)

(provide 'material-purple-theme)

;;; material-purple-theme.el ends here
