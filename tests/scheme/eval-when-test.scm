(import (liii check))
(import (goldfish))

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

;; ----- edge cases -----

;; expand situation can define a macro that later expansion sees
(eval-when (expand)
  (define-syntax from-expand
    (syntax-rules () ((from-expand) 1))))
(check (from-expand) => 1)

;; situation order is irrelevant
(eval-when (eval load) (define ordered 3))
(check ordered => 3)

;; empty situation list: no effect, no error
(define empty-before 1)
(eval-when ())
(check empty-before => 1)

;; eval-when in expression position returns the last expr's value
(define v (eval-when (load eval) 5))
(check v => 5)

;; interaction with cond-expand (r7rs is always satisfied)
(cond-expand (r7rs (eval-when (load eval) (define ce-var 5))))
(check ce-var => 5)

;; nested eval-when: the outer load/eval keeps its define; the inner
;; expand clause runs at expand time, so the macro it defines is visible
;; to later expansion
(eval-when (load eval)
  (eval-when (expand)
    (define-syntax nested-mac (syntax-rules () ((nested-mac) 3))))
  (define nested-ok 1))
(check nested-ok => 1)
(check (nested-mac) => 3)

;; NOTE: an unrecognized situation is an expand-time error:
;;   (eval-when (foo) 1) -> "eval-when: invalid situation"
;; Because it aborts the whole program (all three expansion paths: toplevel,
;; library body, expression position), it cannot be asserted inside a passing
;; test file; it is verified by running a program containing it and checking
;; for the error exit.

(check-report)
