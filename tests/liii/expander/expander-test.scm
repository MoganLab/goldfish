;; expander bootstrap-0 test
;;
;; Exercises the Sets-of-Scopes expander.  The expander is loaded by bin/gf
;; at startup (customize_goldfish_by_mode): the pre-expanded artifact
;; kernel-combined.scm plus the lib layer, for non-s7 modes.  Programs are
;; compiled through compile-program and the result is evaluated by s7 (the
;; host evaluator).
;;
;; Loading: no manual loads here; bin/gf has already installed the expander.

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
;; bytevector literals (#u8(...))
;; =============================================================================
(check (run '((bytevector? #u8(1 2 3)))) => #t)
(check (run '((bytevector-length #u8(1 2 3)))) => 3)
(check (run '((bytevector-u8-ref #u8(1 2 3) 0))) => 1)
(check (run '((equal? #u8(1 2 3) (bytevector 1 2 3)))) => #t)
(check (run '((equal? #u8() (bytevector)))) => #t)
(check (run '((bytevector? #u8()))) => #t)
(check (run '((quote #u8(1 2 3)))) => #u8(1 2 3))
(check (run '((car (list #u8(1 2) #u8(3 4))))) => #u8(1 2))
(check (run '((bytevector? (quote #u8(7))))) => #t)
(check (run '((bytevector-length #u8(255 128 0)))) => 3)
(check (run '((bytevector-u8-ref (bytevector-copy #u8(9 8 7)) 2))) => 7)
(check (run '((equal? (vector-ref #(#u8(1) #u8(2)) 1) #u8(2)))) => #t)
(check (run '((bytevector? (cdr (cons 1 #u8(2 3)))))) => #t)

;; =============================================================================
;; expansion output shape
;; =============================================================================
;; define desugars to letrec*
(check (not (not (memq 'letrec* (run-raw '((define (f x) x)))))) => #t)
