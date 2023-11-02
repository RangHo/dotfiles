;;; language-fasmg.el --- Language support for Flat Assembler G sources

;;; Commentary:

;; Since the FASMG syntax differs significantly from regular x86 assembly syntax,
;; this package is not a simple extension of the `asm-mode' package.
;; Based on Fanael Linithien's `fasm-mode' package, the original
;; source code repository of which is not available anymore.

;;; Code:

(defvar fasmg-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?. "_" table)
    (modify-syntax-entry ?$ "_" table)
    (modify-syntax-entry ?@ "_" table)
    (modify-syntax-entry ?~ "_" table)
    (modify-syntax-entry ?? "_" table)
    (modify-syntax-entry ?! "_" table)
    (modify-syntax-entry ?= "." table)
    (modify-syntax-entry ?+ "." table)
    (modify-syntax-entry ?- "." table)
    (modify-syntax-entry ?* "." table)
    (modify-syntax-entry ?/ "." table)
    (modify-syntax-entry ?\\ "." table)
    (modify-syntax-entry ?\; "<" table)
    (modify-syntax-entry ?\n ">" table)
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?\' "\"" table)
    table)
  "Syntax table for `fasmg-mode'.")

(provide 'language-fasmg)

;;; language-fasmg.el ends here
