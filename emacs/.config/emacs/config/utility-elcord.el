;;; utility-elcord.el --- Discord RPC integration for Emacs

;; Install Discord integration
(use-package elcord)

;; Make elcord shut up
(setq elcord-quiet t)

;; Enable elcord only when focused
(add-hook 'focus-in-hook
          (lambda () (elcord-mode 1)))
(add-hook 'focus-out-hook
          (lambda () (elcord-mode 0)))

(provide 'utility-elcord)
