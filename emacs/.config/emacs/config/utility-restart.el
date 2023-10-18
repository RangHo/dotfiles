;;; utility-restart.el --- Restart Emacs easily

;;; Commentary:

;; This package provides a command to restart Emacs easily.
;; It provides two interactive command:
;;   - `restart-emacs' :: Restart the current Emacs process
;;   - `reload-emacs' :: Reload the Emacs configuration file

;;; Code:

(defun rangho/start-emacs-nw ()
  "Start another Emacs process in no-window mode."
  (suspend-emacs "fg; emacs -nw"))

(defun rangho/start-emacs-graphic ()
  "Start another Emacs process in graphic mode."
  (call-process "sh" nil nil nil "-c" "emacs &"))

(defun restart-emacs ()
  "Shutdown the current Emacs process and start a new one."
  (interactive)
  (let ((kill-emacs-hook (append kill-emacs-hook
                                 (list (if (display-graphic-p)
                                           #'rangho/start-emacs-graphic
                                         #'rangho/start-emacs-nw)))))
    (save-buffers-kill-emacs)))

(defun reload-emacs ()
  "Reload the init.el file."
  (interactive)
  (load-file user-init-file))

(provide 'utility-restart)

;;; utility-restart.el ends here
