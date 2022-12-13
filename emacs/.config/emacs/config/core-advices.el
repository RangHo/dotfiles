;;; core-advices.el --- List of useful advices that make my life easier

;; Delete a line without worrying about excess spaces.
(defun rangho/kill-excess-whitespaces (_)
  "Kill excess whitespace when joining lines.
If the next line is joined to the current line, kill the extra indent whitespace in front of the next line."
  (when (and (eolp) (not (bolp)))
    (save-excursion
      (forward-char 1)
      (just-one-space 1))))

(advice-add 'kill-line :before #'rangho/kill-excess-whitespaces)
(advice-add 'kill-visual-line :before #'rangho/kill-excess-whitespaces)

(provide 'core-advices)
