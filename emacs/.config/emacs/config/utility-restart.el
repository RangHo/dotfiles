;;; utility-restart.el --- Restart Emacs easily

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

(provide 'utility-restart)
