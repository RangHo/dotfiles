;;; core-gc.el --- Garbage collector optimizations

;;; Commentary:

;;

;;; Code:

(defconst rangho/gc-cons-threshold
  (* 64 1024 1024)
  "The default value of `gc-cons-threshold'.")

(defconst rangho/gc-cons-percentage
  0.4
  "The default value of `gc-cons-percentage'.")

(defun rangho/set-excessive-gc ()
  "Set the garbage collector threshold for high-speed usage."
  (setq gc-cons-threshold most-positive-fixnum
	gc-cons-percentage 0.6))

(defun rangho/set-reasonable-gc ()
  "Set the garbage collector threshold for normal usage."
  (setq gc-cons-threshold rangho/gc-cons-threshold
	gc-cons-percentage rangho/gc-cons-percentage))

(defun rangho/collect-garbage-on-focus-change ()
  "Collect garbage when Emacs noticese a focus change event."
  (unless (frame-focus-state)
    (garbage-collect)))

;; Set the default garbage collector threshold
(add-hook 'after-init-hook #'rangho/set-reasonable-gc)

;; Set the garbage collector threshold for minibuffer usage
(add-hook 'minibuffer-setup-hook #'rangho/set-excessive-gc)
(add-hook 'minibuffer-exit-hook #'rangho/set-reasonable-gc)

;; Collect garbage when Emacs notices a focus change event
(add-function :after after-focus-change-function
	      #'rangho/collect-garbage-on-focus-change)

(provide 'core-gc)

;;; core-gc.el ends here
