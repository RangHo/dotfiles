;;; praat-mode.el --- Praat script editing in Emacs

;; Author: Stefan Werner <stefan.werner@iki.fi>
;; Created: 2002-02-19
;; Version: 0.2.2 2006-12-20 (highlighting reduced to keywords,
;;                           commands and objects)
;;
;; Keywords: praat

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;; wwww.gnu.org

;;; Commentary:

;; Sets up text-mode with support for Praat-style "#" comments and
;; probably incomplete Praat (www.praat.org) syntax highlighting.

;; You may wish to add something like the following to your ~/.emacs file
;; (the examples assume that you put praat-mode.el into your ~/emacs directory
;; and that your Praat script files have the suffix ".praat" in their name):
;;   (setq load-path (cons "~/emacs" load-path))
;;   (autoload 'praat-mode "praat" "Enter Praat mode." t)
;;   (setq auto-mode-alist (cons '("\\.praat$" . praat-mode) auto-mode-alist))


;;; Code:

(defvar praat-mode-syntax-table nil
  "Syntax table in use in Praat-mode buffers.")

(if praat-mode-syntax-table
    ()
  (setq praat-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?\\ "\\" praat-mode-syntax-table)
  (modify-syntax-entry ?\n ">   " praat-mode-syntax-table)
  (modify-syntax-entry ?\f ">   " praat-mode-syntax-table)
  (modify-syntax-entry ?\# "<   " praat-mode-syntax-table)
  (modify-syntax-entry ?/ "." praat-mode-syntax-table)
  (modify-syntax-entry ?* "." praat-mode-syntax-table)
  (modify-syntax-entry ?+ "." praat-mode-syntax-table)
  (modify-syntax-entry ?- "." praat-mode-syntax-table)
  (modify-syntax-entry ?= "." praat-mode-syntax-table)
  (modify-syntax-entry ?% "." praat-mode-syntax-table)
  (modify-syntax-entry ?< "." praat-mode-syntax-table)
  (modify-syntax-entry ?> "." praat-mode-syntax-table)
  (modify-syntax-entry ?& "." praat-mode-syntax-table)
  (modify-syntax-entry ?| "." praat-mode-syntax-table)
  (modify-syntax-entry ?$ "." praat-mode-syntax-table)
  (modify-syntax-entry ?_ "_" praat-mode-syntax-table)
  (modify-syntax-entry ?\" "\"" praat-mode-syntax-table)
  (modify-syntax-entry ?\' "'" praat-mode-syntax-table))

;; Regexps for font-lock
(defconst praat-font-lock-keywords
  (eval-when-compile
    (list

     ;; Keywords.
     (list (regexp-opt
      '("break" "continue" "delete" "exit" "else" "for"
	"getline" "if" "next" "print" "printf" "return" "while"
	"call" "clearinfo" "comment" "down" "echo" "editor"
	"else" "elsif" "endeditor" "endfor" "endform" "endif" "endproc"
	"endwhile" "execute" "exit" "for" "form" "from" "if"
	"ifelse" "minus" "no" "pause" "plus" "print" "printline" "printtab"
			"procedure" "repeat" "select" "to" "until" "while")
		 'words) 1 font-lock-keyword-face)

     ;; Builtins.
     (list (regexp-opt
	    '("Add" "Copy" "Create" "Down" "Draw" "Paint"
	"Formula" "Get" "Play" "Query" "Read" "Remove" "Rename" 
	"To" "Write")
	'words) 1 'font-lock-builtin-face)

     ;; Objects.
     (list (regexp-opt
	    '("Articulation" "Artword" "Categories" "Cepstrum" "Cochleagram"
	"Configuration" "Confusion" "Correlation" "Covariance" "DTW"
	"Distributions" "DurationTier" "Eigen" "Excitation" "Excitations"
	"ExperimentMFC" "FFNet" "Filtering" "Formant" "FormantTier"
	"Harmonicity" "Intensity" "IntensityTier" "LPC" "LongSound" "Ltas"
	"Manipulation" "Matrix" "OTGrammar" "PCA" "ParamCurve" "Pattern"
	"Permutation" "Pitch" "PitchTier" "PointProcess" "Polygon"
	"Polynomial" "Sound" "Speaker" "Spectrogram" "Spectrum" "Strings"
	"Table" "TableOfReal" "TextGrid" "VocalTract" "Voice" "WordList")
	'words)   1 'font-lock-function-name-face)

     ;; Operators.
     (cons (regexp-opt '("&&" "||" "<=" "<" ">=" ">" "==" "!=" "!~" "~" "..."))
	   'font-lock-constant-face)
     ))
 "Default expressions to highlight in Praat mode.")

(define-derived-mode praat-mode text-mode "Praat"
  "Major mode for editing Praat code.
This is simply text mode plus automatic un-/commenting of regions and
keyword highlighting via font-lock. To indent (e.g. loop bodies) you
have to press TAB, C-j for the following indented lines and Return to
get back to the left margin - no auto-indentation here, sorry."

  (set (make-local-variable 'paragraph-start) (concat "$\\|" page-delimiter))
  (set (make-local-variable 'paragraph-separate) paragraph-start)
  (set (make-local-variable 'comment-start) "# ")
  (set (make-local-variable 'comment-end) "")
  (set (make-local-variable 'comment-start-skip) "#+ *")
  (setq font-lock-defaults '(praat-font-lock-keywords nil nil ((?_ . "w")))))

(provide 'praat-mode)

;;; praat-mode.el ends here
