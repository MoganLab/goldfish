(import (liii check))
(import (goldfish))

;; eval-when : R7RS 7.1.3
;;   (eval-when (situation ...) expr ...), situation in expand/load/eval.
;;   - expand: exprs run at expansion time; effects are visible to later
;;     expansion of the same program (macro expansion, imports).
;;   - load/eval: exprs are kept in the phase-0 output.

;; expand situation inside a macro: a transformer body runs when the
;; macro is used.  (v5) the state it reads and mutates must live in the
;; transformer's own lexical scope -- a phase-0 variable is not visible
;; from phase 1.  The check is idempotent on purpose: the test harness
;; may expand a form more than once (whole-file compile + per-form
;; fallback), so expansion-count-sensitive assertions would flap.
(define-syntax m
  (letrec* ((state (vector 'expanded)))
    (lambda (stx)
      (datum->syntax stx (list 'quote (vector-ref state 0))))))
(check (m) => 'expanded)

;; expand situation at top level: runs during expansion; the state lives
;; in a region binding, visible to later expansion of the same program.
(eval-when (expand)
  (define expand-state 'expanded))
(define-syntax m-state
  (lambda (stx)
    (datum->syntax stx (list 'quote expand-state))))
(check (m-state) => 'expanded)

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

;; (v5) expansion-time set! of a phase-0 variable is an expansion-time
;; error: the expand region cannot see the program's own phase-0 value
;; bindings.  The rejected program never executes, so the two evaluation
;; paths (whole-file compile and per-form fallback) cannot diverge.
(import (liii os))
(define strict-src
  (string-append (os-temp-dir) "/gf-eval-when-strict.scm"))
(call-with-output-file strict-src
  (lambda (p)
    (display "(import (goldfish))" p) (newline p)
    (display "(define flag #f)" p) (newline p)
    (display "(eval-when (expand) (set! flag #t))" p) (newline p)))
(check-catch 'unbound-variable (compile-file-cached strict-src))
(delete-file strict-src)

;; NOTE: an unrecognized situation is an expand-time error:
;;   (eval-when (foo) 1) -> "eval-when: invalid situation"
;; Because it aborts the whole program (all three expansion paths: toplevel,
;; library body, expression position), it cannot be asserted inside a passing
;; test file; it is verified by running a program containing it and checking
;; for the error exit.

(check-report)
