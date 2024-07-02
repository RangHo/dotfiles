;;; utility-elcord.el --- Discord RPC integration for Emacs

;;; Commentary:

;; This package installs `elcord' minor mode to to interface with Discord.

;;; Code:

(defconst rangho/elcord-grace-period 60
  "Time in seconds to wait before disconnecting from Discord.")

(defun rangho/connect-elcord-on-focus-change ()
  "Connect to Discord when Emacs notices a focus change event."
  (when elcord-mode
    (if (frame-focus-state)
	(elcord--enable)
      (run-with-idle-timer rangho/elcord-grace-period nil #'elcord--disable))))

;; Register focus hook
(add-function :after after-focus-change-function
	      #'rangho/connect-elcord-on-focus-change)

;; Install Discord integration
(use-package elcord
  :custom
  (elcord-use-major-mode-as-main-icon t))

(provide 'utility-elcord)

;;; utility-elcord.el ends here
