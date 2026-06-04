;;; treesit-predicate-rewrite.el --- -*- lexical-binding: t -*-
;;
;; Description: Generic workaround for Emacs bug#79687 -- strip
;;  tree-sitter query predicates at `treesit-font-lock-rules' call
;;  time, replacing them with capture-name-is-a-function fontifiers.
;; Author: Michael Olson
;;
;; The contents of this file may be used, distributed, and modified
;; without restriction.

;;; Commentary:

;; Emacs 30.2 serialises `:match' / `:equal' / `:pred' s-expr
;; predicates to `#match' / `#equal' / `#pred' (no trailing `?'), and
;; libtree-sitter >=0.26 rejects those at query-parse time. The
;; upstream fix (commit b0143530 on master) is slated for Emacs 31 and
;; has not been backported to emacs-30 as of 30.2. On Arch we ship
;; libtree-sitter.so.0.26, so every mode whose `treesit-font-lock-rules'
;; uses `:match' fails the first time it hits tree-sitter. `#match?'
;; doesn't work either: Emacs 30.2's own predicate dispatcher hard-codes
;; bare `match'/`equal'/`pred' and rejects `match?'/`equal?'/`pred?' at
;; eval time, so no string-level rewrite can satisfy both sides.
;;
;; Instead, intercept `treesit-font-lock-rules' via `:filter-args'
;; advice and rewrite its argument plist, replacing predicate-gated
;; patterns with capture-name-is-a-function fontifiers. The resulting
;; query contains NO predicates, so libtree-sitter is happy; the
;; fontifier applies the face only when the node satisfies the
;; original predicate, so the semantics are preserved.
;;
;; `treesit-range-rules' (used by e.g. js.el for JSDoc parser embed)
;; gets the same advice, but range captures are scratch names rather
;; than faces, so predicates there always strip cleanly (Case C). The
;; downside is slight over-matching (e.g. JSDoc embed attaches to all
;; comments, not just `/**' ones); benign in practice.
;;
;; Handled cases (in order of detection):
;;
;;   A. ((NODE) @FACE (:match REGEX @FACE))
;;      -> ((NODE) @my-ts-rw--fn-FACE-HASH), and the fontifier applies
;;      FACE when the node text matches REGEX. Also handles nested
;;      captures like ((X Y: (Z) @FACE) (:match ... @FACE)).
;;
;;   B. (((NODE) @scratch (:match REGEX @scratch)) @FACE)
;;      Outer-group-wraps-inner-scratch form (used by ruby-ts-mode
;;      builtins). Semantically equivalent to Case A after flattening,
;;      handled the same way.
;;
;;   C. Predicate where target capture is not the face capture, e.g.,
;;      elixir-ts-mode's (call (identifier) @_fn (:match "assert" @_fn)
;;      arguments: ...). Cannot be flattened into a single fontifier
;;      safely; the predicate is silently stripped (set
;;      `my-ts-rw-verbose' non-nil to log strips). This will
;;      over-fontify, which is a minor visual regression. elixir-ts-mode
;;      uses this pattern heavily and is best left alone -- callers
;;      should avoid loading it with this advice active, OR accept
;;      the over-fontification.
;;
;; :equal (@cap "literal") is handled for Case A/B; :pred is not
;; (would require re-invoking the user's fn) and is treated as Case C.
;;
;; Removal plan: delete this whole file and its `load' in
;; `shared-init.el' once we upgrade to an Emacs that carries the
;; bug#79687 fix (Emacs 31+, or a backport landing in a future 30.x
;; point release).

;;; Code:

(require 'cl-lib)
(require 'seq)

(defvar my-ts-rw--fontifiers (make-hash-table :test 'equal)
  "Cache: (KIND FACE . DATA) -> interned fontifier symbol.
KIND is `regex' or `equal'. DATA is the regex string or literal.
Prevents symbol churn when the same rewrite is computed repeatedly
across reloads.")

(defvar my-ts-rw-verbose nil
  "When non-nil, log each predicate rewrite or strip via `message'.
Off by default so Case C over-fontification notes don't spam the
echo area at mode-load time.")

;;;; Entry points: :filter-args advice on query-building functions

(define-advice treesit-font-lock-rules
    (:filter-args (args) my-ts-rw-rewrite)
  "Rewrite tree-sitter predicates into fontifier captures.
Workaround for Emacs bug#79687 / libtree-sitter 0.26 strict predicates."
  (condition-case err
      (my-ts-rw--rewrite-args args)
    (error
     (message "treesit-predicate-rewrite: rewrite failed, passing through: %S" err)
     args)))

(define-advice treesit-range-rules
    (:filter-args (args) my-ts-rw-rewrite)
  "Strip tree-sitter predicates from range rules.
Range-rule captures are scratch names, not faces, so predicates
always fall into Case C (strip). The net effect for e.g. js.el's
JSDoc-begin predicate is that the embedded parser attaches to all
comments rather than just `/**' comments, which is harmless but
may cost a little extra parser work on non-doc comments."
  (condition-case err
      (my-ts-rw--rewrite-args args)
    (error
     (message "treesit-predicate-rewrite: range-rules rewrite failed, passing through: %S" err)
     args)))

(define-advice treesit-query-compile
    (:filter-args (args) my-ts-rw-rewrite)
  "Strip/rewrite predicates from queries compiled directly.
Catches call sites that bypass `treesit-font-lock-rules' and
`treesit-range-rules' -- e.g. c-ts-mode's `c-ts-mode--emacs-c-range-query'
defvar which calls `treesit-query-compile' with an s-expression
containing `:match'. Predicates on face targets become fontifiers;
others are stripped."
  (condition-case err
      (pcase args
        (`(,lang ,query . ,rest)
         (cons lang
               (cons (if (listp query)
                         (my-ts-rw--rewrite-query-list query)
                       query)
                     rest)))
        (_ args))
    (error
     (message "treesit-predicate-rewrite: query-compile rewrite failed, passing through: %S" err)
     args)))

;;;; Arg-plist walker

(defun my-ts-rw--rewrite-args (args)
  "Walk ARGS (the plist-style argument list of `treesit-font-lock-rules').
Rewrite every query-list value; leave :KEYWORD/VALUE pairs as-is."
  (let ((result nil)
        (tail args))
    (while tail
      (let ((item (car tail)))
        (cond
         ((keywordp item)
          (push item result)
          (push (cadr tail) result)
          (setq tail (cddr tail)))
         (t
          (push (my-ts-rw--rewrite-query-list item) result)
          (setq tail (cdr tail))))))
    (nreverse result)))

(defun my-ts-rw--rewrite-query-list (queries)
  "Rewrite each top-level query in QUERIES.
QUERIES is a list of queries, or an already-compiled query object /
string; non-list inputs are returned unchanged."
  (if (and (listp queries) (not (null queries)))
      (mapcar #'my-ts-rw--walk queries)
    queries))

;;;; Per-query recursive walker

(defun my-ts-rw--walk (form)
  "Rewrite predicates within FORM recursively."
  (cond
   ((not (consp form)) form)
   ;; Don't descend into a predicate form itself -- its children
   ;; shouldn't be interpreted as predicate-containing queries.
   ((my-ts-rw--predicate-p form) form)
   (t
    ;; Case B must be detected at this level BEFORE recursing into
    ;; children; otherwise the bottom-up recursion would strip the
    ;; inner predicate under Case C and we'd over-fontify.
    (let ((b (my-ts-rw--try-case-b form)))
      (if b
          (my-ts-rw--walk b)
        ;; Bottom-up: recurse children first, then handle Case A at
        ;; this level so inner predicates are resolved before outer.
        (let ((children (mapcar #'my-ts-rw--walk form)))
          (my-ts-rw--rewrite-case-a children)))))))

;;;; Case B: (((NODE) @scratch (:match REGEX @scratch)) @FACE)

(defun my-ts-rw--try-case-b (form)
  "If FORM matches Case B, return the pre-rewrite flattened form.
Otherwise return nil. The returned form still has the inner
predicate in place; Case A handling will lift it."
  (when (and (consp form)
             (= (length form) 2)
             (consp (car form))
             (my-ts-rw--face-symbol-p (cadr form)))
    (let* ((group (car form))
           (outer-face-sym (cadr form))
           (predicates (seq-filter #'my-ts-rw--predicate-p group)))
      (when (= (length predicates) 1)
        (let* ((pred (car predicates))
               (target (my-ts-rw--predicate-target pred)))
          (when (and target
                     (my-ts-rw--cap-symbol-p target)
                     ;; Case B is only safe if the inner scratch capture
                     ;; tags the same node the outer face wraps -- if
                     ;; target appears as a direct capture marker inside
                     ;; group (not nested inside a named-child pattern),
                     ;; flattening is semantically sound.
                     (my-ts-rw--cap-directly-on-root-of-p group target)
                     ;; Avoid double-rewriting if target already is a face
                     ;; (that's Case A in disguise, handled below).
                     (not (my-ts-rw--face-symbol-p target)))
            ;; Substitute the inner scratch capture with the outer
            ;; face symbol and drop the outer wrapping. Case A will
            ;; then recognise the predicate + face pairing.
            (my-ts-rw--replace-capture-in-tree
             group target outer-face-sym)))))))

(defun my-ts-rw--cap-directly-on-root-of-p (group cap-sym)
  "Non-nil if CAP-SYM appears as a top-level capture marker in GROUP.
Specifically, CAP-SYM is a direct element of GROUP that follows the
node pattern (i.e., `(NODE CAP-SYM ...)'). This is the signature of
the inner scratch in Case B."
  (and (consp group)
       (memq cap-sym group)
       t))

;;;; Case A: ((...@FACE...) (:match REGEX @FACE))

(defun my-ts-rw--rewrite-case-a (children)
  "Process top-level predicates among CHILDREN.
Returns the rewritten list with predicates stripped and capture
symbols rewritten to fontifier symbols where applicable."
  (if (not (cl-some #'my-ts-rw--predicate-p children))
      children
    (let ((result children))
      (dolist (pred (seq-filter #'my-ts-rw--predicate-p children))
        (let ((target (my-ts-rw--predicate-target pred)))
          (cond
           ((and target (my-ts-rw--face-symbol-p target))
            (let* ((face (my-ts-rw--cap-to-face target))
                   (fontifier (my-ts-rw--try-make-fontifier pred face)))
              (if fontifier
                  (setq result
                        (my-ts-rw--replace-capture-in-tree
                         result target (my-ts-rw--face-to-cap fontifier)))
                (when my-ts-rw-verbose
                  (message "treesit-predicate-rewrite: unsupported predicate %S (face target %s); stripping"
                           (car pred) target)))))
           (t
            (when my-ts-rw-verbose
              (message "treesit-predicate-rewrite: stripping non-face-target predicate %S"
                       pred))))))
      (seq-remove #'my-ts-rw--predicate-p result))))

;;;; Fontifier factory

(defun my-ts-rw--try-make-fontifier (predicate face)
  "Build a fontifier applying FACE when PREDICATE is satisfied.
Return the fontifier symbol, or nil if PREDICATE isn't supported."
  (pcase predicate
    (`(:match ,(and regex (pred stringp)) ,(pred my-ts-rw--cap-symbol-p))
     (my-ts-rw--intern-fontifier
      (list 'regex face regex)
      (lambda (node override start end &rest _)
        (when (string-match-p regex (treesit-node-text node t))
          (treesit-fontify-with-override
           (treesit-node-start node) (treesit-node-end node)
           face override start end)))))
    (`(:equal ,a ,b)
     (let ((literal (cond ((and (stringp a) (my-ts-rw--cap-symbol-p b)) a)
                          ((and (my-ts-rw--cap-symbol-p a) (stringp b)) b))))
       (when literal
         (my-ts-rw--intern-fontifier
          (list 'equal face literal)
          (lambda (node override start end &rest _)
            (when (equal (treesit-node-text node t) literal)
              (treesit-fontify-with-override
               (treesit-node-start node) (treesit-node-end node)
               face override start end)))))))
    (_ nil)))

(defun my-ts-rw--intern-fontifier (key fn)
  "Return a fontifier symbol for KEY, defining it to FN on first sight.
Subsequent calls with an `equal' KEY return the cached symbol and do
not redefine it, so reloading this file does not leak symbols."
  (or (gethash key my-ts-rw--fontifiers)
      (let* ((face (cadr key))
             (sym (intern (format "my-ts-rw--fn-%s-%08x"
                                  face (abs (sxhash-equal key))))))
        (defalias sym fn)
        (puthash key sym my-ts-rw--fontifiers)
        sym)))

;;;; Predicate / capture helpers

(defun my-ts-rw--predicate-p (form)
  "Non-nil if FORM is a tree-sitter predicate form."
  (and (consp form)
       (memq (car form) '(:match :equal :pred))))

(defun my-ts-rw--predicate-target (predicate)
  "Return the capture symbol PREDICATE tests, or nil."
  (pcase (car predicate)
    (:match
     (let ((tail (car (last predicate))))
       (and (my-ts-rw--cap-symbol-p tail) tail)))
    (:equal
     (cl-loop for arg in (cdr predicate)
              when (my-ts-rw--cap-symbol-p arg) return arg))
    (:pred
     (cl-loop for arg in (cddr predicate)
              when (my-ts-rw--cap-symbol-p arg) return arg))))

(defun my-ts-rw--cap-symbol-p (x)
  "Non-nil if X is a tree-sitter capture symbol (starts with `@')."
  (and (symbolp x)
       (let ((name (symbol-name x)))
         (and (> (length name) 1) (eq (aref name 0) ?@)))))

(defun my-ts-rw--face-symbol-p (cap-sym)
  "Non-nil if CAP-SYM names a font-lock face.
Accepts any symbol starting with `@' whose trailing name is either
a defined face or begins with `font-lock-'."
  (and (my-ts-rw--cap-symbol-p cap-sym)
       (let ((stripped (substring (symbol-name cap-sym) 1)))
         (or (facep (intern-soft stripped))
             (string-prefix-p "font-lock-" stripped)))))

(defun my-ts-rw--cap-to-face (cap-sym)
  "Return the face symbol underlying capture symbol CAP-SYM."
  (intern (substring (symbol-name cap-sym) 1)))

(defun my-ts-rw--face-to-cap (face-sym)
  "Return the capture symbol (`@face-sym') for FACE-SYM."
  (intern (format "@%s" face-sym)))

;;;; Tree substitution

(defun my-ts-rw--replace-capture-in-tree (tree from to)
  "Recursively replace occurrences of FROM with TO in TREE."
  (cond
   ((eq tree from) to)
   ((consp tree)
    (cons (my-ts-rw--replace-capture-in-tree (car tree) from to)
          (my-ts-rw--replace-capture-in-tree (cdr tree) from to)))
   (t tree)))

(provide 'treesit-predicate-rewrite)
;;; treesit-predicate-rewrite.el ends here
