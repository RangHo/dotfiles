;;; framework-mise.el --- Basic framework for Mise

;;; Commentary:

;;

;;; Code:

(require 'json)
(require 'seq)

(defgroup mise nil
  "Settings to interact with Mise."
  :group 'applications)

(defcustom mise-bin (expand-file-name "~/.local/bin/mise")
  "Path to the Mise binary."
  :type 'string
  :group 'mise)

(defun mise--run (&rest args)
  "Run Mise with optional ARGS."
  (if (file-executable-p mise-bin)
      (shell-command-to-string (string-join (cons mise-bin args) " "))
    nil))

(defun mise--env ()
  "Get environment variables from Mise."
  (let ((mise-env-json-string (mise--run "env" "--json")))
    (if mise-env-json-string
        (json-read-from-string mise-env-json-string)
      nil)))

(defun mise-enable ()
  "Enable Mise for current Emacs session."
  (interactive)
  (let ((mise-env-json (mise--env)))
    (mapc (lambda (env)
            (setenv (symbol-name (car env)) (cdr env)))
          mise-env-json)
    (setq exec-path (parse-colon-path (cdr (assoc (intern "PATH") mise-env-json))))
    (message "Mise enabled.")))

(defun mise-ensure ()
  "Ensure Mise is enabled for current Emacs session."
  (interactive)
  (unless (seq-reduce (lambda (acc env)
                        (and acc (equal (getenv (symbol-name (car env))) (cdr env))))
                      (mise--env)
                      t)
    (mise-enable)))

(mise-ensure)

(provide 'framework-mise)

;;; framework-mise.el ends here
