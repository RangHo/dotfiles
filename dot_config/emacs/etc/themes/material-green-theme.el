;;; material-green-theme.el --- Green-colored Emacs colorscheme

;;; Commentary:

;;; Code:

(require 'rangho-themes)

(deftheme material-green "Green-colored Material Design theme.")

(rangho-themes-set-faces
 'material-green
 '((primary                   . "#5a631e")
   (on-primary                . "#ffffff")
   (primary-container         . "#dfe995")
   (on-primary-container      . "#434b06")
   (secondary                 . "#5d6145")
   (on-secondary              . "#ffffff")
   (secondary-container       . "#e3e5c2")
   (on-secondary-container    . "#46492f")
   (tertiary                  . "#3b665b")
   (on-tertiary               . "#ffffff")
   (tertiary-container        . "#bdecde")
   (on-tertiary-container     . "#224e44")
   (error                     . "#ba1a1a")
   (on-error                  . "#ffffff")
   (error-container           . "#FFDAD6")
   (on-error-container        . "#93000A")
   (surface                   . "#FCFAED")
   (surface-container-lowest  . "#FFFFFF")
   (surface-container-low     . "#F6F4E7")
   (surface-container         . "#F0EEE1")
   (surface-container-high    . "#EAE9DC")
   (surface-container-highest . "#E4E3D6")
   (on-surface                . "#1B1C14")
   (on-surface-variant        . "#47483B")
   (outline                   . "#78786A")
   (outline-variant           . "#C8C7B7")
   (inverse-surface           . "#303128")
   (inverse-on-surface        . "#F3F1E4")
   (inverse-primary           . "#C2CD7C")
   (scrim                     . "#000000")
   (shadow                    . "#000000")))

(provide-theme 'material-green)

(provide 'material-green-theme)

;;; material-green-theme.el ends here
