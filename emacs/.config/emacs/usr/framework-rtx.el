;;; framework-rtx.el --- Basic framework for RunTime eXecutor

;;; Commentary:

;;

;;; Code:

(require 'json)
(require 'seq)

(defgroup rtx nil
  "Settings to interact with RunTime eXecutor."
  :group 'applications)

(defcustom rtx-bin (expand-file-name "~/.local/bin/rtx")
  "Path to the RunTime eXecutor binary."
  :type 'string
  :group 'rtx)

(defun rtx--run (&rest args)
  "Run RunTime eXecutor with optional ARGS."
  (if (file-executable-p rtx-bin)
      (shell-command-to-string (string-join (cons rtx-bin args) " "))
    nil))

(defun rtx--env ()
  "Get environment variables from RunTime eXecutor."
  (let ((rtx-env-json-string (rtx--run "env" "--json")))
    (if rtx-env-json-string
        (json-read-from-string rtx-env-json-string)
      nil)))

(defun rtx-enable ()
  "Enable RunTime eXecutor for current Emacs session."
  (interactive)
  (let ((rtx-env-json (rtx--env)))
    (mapc (lambda (env)
            (setenv (symbol-name (car env)) (cdr env)))
          rtx-env-json)
    (setq exec-path (parse-colon-path (cdr (assoc (intern "PATH") rtx-env-json))))
    (message "RunTime eXecutor enabled.")))

(defun rtx-ensure ()
  "Ensure RunTime eXecutor is enabled for current Emacs session."
  (interactive)
  (unless (seq-reduce (lambda (acc env)
                        (and acc (equal (getenv (symbol-name (car env))) (cdr env))))
                      (rtx--env)
                      t)
    (rtx-enable)))

(rtx-ensure)

(provide 'framework-rtx)

;;; framework-rtx.el ends here
