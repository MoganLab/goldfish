;; expander bootstrap-0 test
;;
;; Exercises the Sets-of-Scopes expander through both bootstrap paths:
;;   1. from-source: s7 evaluates the kernel sources directly
;;   2. from-artifact: the pre-expanded kernel-combined.scm is the expander
;; The expander compiles programs through compile-program and the result is
;; evaluated by s7 (the host evaluator).
;;
;; Loading: this test first loads the seed (liii/boot.scm) and the Scheme
;; reader, then the expander (artifact path here; see the comment in the
;; body for switching to from-source).

(load "liii/boot.scm")
(load "liii/reader.scm")

;; Choose the bootstrap path.  The artifact is the committed pre-expanded
;; expander; the from-source path re-evaluates the kernel sources.  Both
;; must produce a working expander.
(if (and (getenv "EXPANDER_FROM_SOURCE") (string=? (getenv "EXPANDER_FROM_SOURCE") "1"))
  (load-source-file "expander/kernel/load-kernel.scm")
  (begin
    (load-source-file "expander/kernel-combined.scm")
    (load-source-file "expander/lib/install.scm")))
(install-standard-library!)

(define (run prog)
  (eval (compile-program prog) (rootlet)))

(define (run-raw prog)
  (compile-program prog))

(import (liii check))
(check-set-mode! 'report-failed)

;; =============================================================================
;; core expansion
;; =============================================================================
(check (run '((+ 40 2))) => 42)
(check (run '((* 6 7))) => 42)
(check (run '((if #t 1 2))) => 1)
(check (run '((if #f 1 2))) => 2)
(check (run '((quote (1 2 3)))) => '(1 2 3))

;; define + lambda
(check (run '((define (sq x) (* x x)) (sq 7))) => 49)
(check (run '((define x 5) (set! x 6) x)) => 6)

;; =============================================================================
;; R7RS derived forms (standard layer)
;; =============================================================================
(check (run '((let ((a 1) (b 2)) (+ a b)))) => 3)
(check (run '((let* ((a 1) (b (+ a 1))) (+ a b)))) => 3)
(check (run '((letrec ((f (lambda (n) (if (= n 0) 1 (* n (f (- n 1))))))) (f 5)))) => 120)
(check (run '((cond (#f 1) (else 2)))) => 2)
(check (run '((and 1 2 3))) => 3)
(check (run '((or #f #f 7))) => 7)
(check (run '((do ((i 0 (+ i 1))) ((= i 5) i)))) => 5)
(check (run '((let-values (((a b) (values 1 2))) (+ a b)))) => 3)

;; =============================================================================
;; macros: syntax-rules
;; =============================================================================
(check (run '((define-syntax my-let
                (syntax-rules ()
                  ((_ var init . body) (let ((var init)) (begin . body)))))
              (my-let x 5 (+ x 1))))
  => 6)

;; hygiene: macro-introduced binding does not capture user variable
(check (run '((define-syntax my-or
                (syntax-rules ()
                  ((_ a b) (let ((t a)) (if t t b)))))
              (let ((t 100))
                (my-or #f t))))
  => 100)

;; =============================================================================
;; macros: define-record-type (from lib layer)
;; =============================================================================
(check (run '((define-record-type point (make-point x y) point? (x point-x) (y point-y))
              (define p (make-point 3 4))
              (+ (point-x p) (point-y p))))
  => 7)

;; =============================================================================
;; macros: syntax-case (procedural transformer)
;; =============================================================================
(check (run '((letrec-syntax
                ((add (lambda (stx)
                        (syntax-case stx ()
                          ((_ a b) (syntax (+ a b)))))))
                (add 1 2))))
  => 3)

;; =============================================================================
;; expansion output shape
;; =============================================================================
;; define desugars to letrec*
(check (not (not (memq 'letrec* (run-raw '((define (f x) x)))))) => #t)
