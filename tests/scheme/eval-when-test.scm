(import (liii check))

;; eval-when : R7RS 7.1.3
;;   (eval-when (situation ...) expr ...), situation in expand/load/eval.
;;   - expand: exprs run at expansion time; effects are visible to later
;;     expansion of the same program (macro expansion, imports).
;;   - load/eval: exprs are kept in the phase-0 output.

;; expand situation inside a macro: runs when the macro is used.
(define expand-side-effect #f)
(define-syntax m
  (lambda (stx)
    (eval-when (expand)
      (set! expand-side-effect #t))
    (datum->syntax stx '(quote ok))))
(check (m) => 'ok)
(check expand-side-effect => #t)

;; expand situation at top level: runs during expansion.
(eval-when (expand)
  (set! expand-side-effect 'expanded))
(check expand-side-effect => 'expanded)

;; load/eval situation: kept in phase-0 output (a definition is legal).
(eval-when (load eval)
  (define loaded-value 42))
(check loaded-value => 42)

;; combined: expand runs now, load/eval keeps the expr.
(eval-when (expand load eval)
  (define combined 7))
(check combined => 7)

(check-report)
