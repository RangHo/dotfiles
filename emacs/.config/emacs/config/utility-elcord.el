;;; utility-elcord.el --- Discord RPC integration for Emacs

;;; Commentary:

;; This package installs `elcord' minor mode to to interface with Discord.

;;; Code:

;; Install Discord integration
(use-package elcord)

;; Make elcord shut up
(setq elcord-quiet t)

;; Enable elcord only when focused
(add-function :after after-focus-change-function
              (lambda ()
                (if (frame-focus-state)
                    (elcord-mode 1)
                  (elcord-mode -1))))

(provide 'utility-elcord)

;;; utility-elcord.el ends here
