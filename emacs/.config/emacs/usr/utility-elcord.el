;;; utility-elcord.el --- Discord RPC integration for Emacs

;;; Commentary:

;; This package installs `elcord' minor mode to to interface with Discord.

;;; Code:

(defvar rangho/elcord-focus-out-timer nil
  "Timer to automatically disconnect from Discord after a period of inactivity.")

(defun rangho/elcord-focus-out ()
  "Disconnect from Discord after a period of inactivity."
  (interactive)
  (when (and (bound-and-true-p elcord-mode)
             (not rangho/elcord-focus-out-timer))
    (setq rangho/elcord-focus-out-timer
          (run-with-timer 60
                          nil
                          (lambda ()
                            (elcord-mode -1)
                            (setq rangho/elcord-focus-out-timer nil))))))

(defun rangho/elcord-focus-in ()
  "Reconnect to Discord when Emacs is focused."
  (interactive)
  (cond
   ;; If elcord is running and the timer is active, cancel it
   ((and (bound-and-true-p elcord-mode)
         rangho/elcord-focus-out-timer)
    (cancel-timer rangho/elcord-focus-out-timer)
    (setq rangho/elcord-focus-out-timer nil))

   ;; If elcord is not running, start it
   ((not elcord-mode)
    (elcord-mode 1)
    (setq rangho/elcord-focus-out-timer nil))))

;; Register focus hooks
(add-hook 'focus-out-hook #'rangho/elcord-focus-out)
(add-hook 'focus-in-hook #'rangho/elcord-focus-in)

;; Install Discord integration
(use-package elcord
  :custom
  (elcord-use-major-mode-as-main-icon t))

(provide 'utility-elcord)

;;; utility-elcord.el ends here
