;;; hades.el --- Evil-integrated God mode

;; Copyright (C) 2026 RangHo Lee

;; Author: RangHo Lee <hello@rangho.me>
;; URL: https://github.com/RangHo/dotfiles
;; Package-Requires: ((emacs "24.4") (evil "1.14.2") (god-mode "2.19.0"))

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; "Hades" state is an evil-mode state that integrates with `god-mode' to allow
;; executing keybindings without having to hold down modifier keys.

;;; Code:

(require 'evil)
(require 'god-mode)

(defvar hades--current-buffer nil
  "The buffer that Hades state is activated.")

(defvar hades--last-command nil
  "Last command executed before entering hades state.")

(defun hades--enter ()
  "Enter god mode; used as evil mode entry hook."
  (god-local-mode +1))

(defun hades--exit ()
  "Exit god mode; used as evil mode exit hook."
  (god-local-mode -1))

(defun hades--fix-last-command ()
  "Fix `last-command' before entering Hades mode."
  (setq last-command hades--last-command))

(defun hades-start-execute-in-hades-state ()
  "Execute the next command in Hades state."
  (interactive)
  ;; Setup hooks for one-shot execution
  (add-hook 'pre-command-hook #'hades--fix-last-command)
  (add-hook 'post-command-hook #'hades-stop-execute-in-hades-state)
  ;; Backup variables
  (setq hades--current-buffer (current-buffer))
  (setq hades--last-command last-command)
  ;; If visual state, then preserve the mark and the point
  (if (evil-visual-state-p)
      (let ((mark-backup (mark))
            (point-backup (point)))
        (evil-hades-state)
        (set-mark mark-backup)
        (goto-char point-backup))
    (evil-hades-state))
  (evil-echo "Switched to Hades state for next command..."))

(defun hades-stop-execute-in-hades-state ()
  "Stop executing commands in Hades state."
  (interactive)
  ;; Detect when a God mode command is completed
  (unless (or (eq this-command #'hades-start-execute-in-hades-state)
              (eq this-command #'universal-argument)
              (eq this-command #'universal-argument-minus)
              (eq this-command #'universal-argument-more)
              (eq this-command #'universal-argument-other-key)
              (eq this-command #'digit-argument)
              (eq this-command #'negative-argument)
              (minibufferp))
    ;; Remove hooks
    (remove-hook 'pre-command-hook #'hades--fix-last-command)
    (remove-hook 'post-command-hook #'hades-stop-execute-in-hades-state)
    ;; Exit Hades state for the buffer
    (when (buffer-live-p hades--current-buffer)
      (with-current-buffer hades--current-buffer
        (if (and (eq evil-previous-state 'visual)
                 (not (use-region-p)))
            (progn
              (evil-change-to-previous-state)
              (evil-exit-visual-state))
          (evil-change-to-previous-state))))
    ;; Reset buffer backup
    (setq hades--current-buffer nil)))

(defun hades-cancel ()
  "Cancel the Hades state and return to normal state."
  (interactive)
  ;; Cleanup
  (hades-stop-execute-in-hades-state)
  (hades--exit)
  (evil-normal-state))

;;;###autoload
(evil-define-state hades
  "God mode."
  :tag " <H> "
  :message "-- HADES --"
  :entry-hook (hades--enter)
  :exit-hook (hades--exit)
  :input-method t
  :intercept-esc nil)

(provide 'hades)
;;; hades.el ends here
