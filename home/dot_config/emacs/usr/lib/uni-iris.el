;;; uni-iris.el --- Unicode input method for Iris framework -*- lexical-binding: t; -*-
;; Copyright (C) 2018--2025 Iris developers and contributors
;; Copyright (C) 2026 RangHo Lee

;; Author: RangHo Lee <hello@rangho.me>
;; URL: https://github.com/RangHo/dotfiles
;; Package-Requires: ((math-symbols-list "1.3") (emacs "29.1"))

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; Input methods for inputting Unicode symbols extensively used in Iris
;; framework, an Rocq implementation of separation logic.

;;; Code:

(require 'quail)

(quail-define-package
 "uni-iris" "UTF-8" "Ω" t
 "Unicode input method specifically tailored for editing Iris framework code.
This is originally nicked from the official Iris repository.  See
https://gitlab.mpi-sws.org/iris/iris/-/blob/master/docs/editor.md for more
information.")

(eval-when-compile
  (require 'math-symbol-lists)
  (defmacro uni-iris--define-rules ()
    `(quail-define-rules
      ,@(let ((rules nil))
          (mapcar (lambda (x))))))
  )

(quail-define-rules
 ;; Commonly used symbols.
 ("\\fun"    ?λ)
 ("\\mult"   ?⋅)
 ("\\ent"    ?⊢)
 ("\\valid"  ?✓)
 ("\\diamond" ?◇)
 ("\\box"    ?□)
 ("\\bbox"   ?■)
 ("\\later"  ?▷)
 ("\\pred"   ?φ)
 ("\\and"    ?∧)
 ("\\or"     ?∨)
 ("\\comp"   ?∘)
 ("\\ccomp"  ?◎)
 ("\\all"    ?∀)
 ("\\ex"     ?∃)
 ("\\to"     ?→)
 ("\\sep"    ?∗)
 ("\\lc"     ?⌜)
 ("\\rc"     ?⌝)
 ("\\Lc"     ?⎡)
 ("\\Rc"     ?⎤)
 ("\\lam"    ?λ)
 ("\\empty"  ?∅)
 ("\\Lam"    ?Λ)
 ("\\Sig"    ?Σ)
 ("\\-"      ?∖)
 ("\\aa"     ?●)
 ("\\af"     ?◯)
 ("\\auth"   ?●)
 ("\\frag"   ?◯)
 ("\\iff"    ?↔)
 ("\\gname"  ?γ)
 ("\\incl"   ?≼)
 ("\\latert" ?▶)
 ("\\update" ?⇝)
 ;; Accents.
 ("\\\"o" ?ö)
 ;; Subscripts and superscripts.
 ("^^+" ?⁺)
 ("__+" ?₊)
 ("^^-" ?⁻)
 ("__0" ?₀)
 ("__1" ?₁)
 ("__2" ?₂)
 ("__3" ?₃)
 ("__4" ?₄)
 ("__5" ?₅)
 ("__6" ?₆)
 ("__7" ?₇)
 ("__8" ?₈)
 ("__9" ?₉)
 ("__a" ?ₐ)
 ("__e" ?ₑ)
 ("__h" ?ₕ)
 ("__i" ?ᵢ)
 ("__k" ?ₖ)
 ("__l" ?ₗ)
 ("__m" ?ₘ)
 ("__n" ?ₙ)
 ("__o" ?ₒ)
 ("__p" ?ₚ)
 ("__r" ?ᵣ)
 ("__s" ?ₛ)
 ("__t" ?ₜ)
 ("__u" ?ᵤ)
 ("__v" ?ᵥ)
 ("__x" ?ₓ))



(provide 'uni-iris)
;;; uni-iris.el ends here
