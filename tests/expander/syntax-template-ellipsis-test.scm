(import (liii check) (scheme base) (srfi srfi-165))

;; Template ellipsis semantics: a pattern or with-syntax variable bound ONCE
;; (not by the ellipsis being repeated) may appear inside that ellipsis's
;; template element; it stays constant across every repeat.  Guile and Racket
;; expand (scalar-in-ellipsis (10 20) 99) to ((10 99) (20 99)); goldfish used
;; to crash the instantiation by indexing the scalar as a repeated variable.

(define-syntax scalar-in-ellipsis
  (lambda (stx)
    (syntax-case stx ()
      ((_ (var ...) k)
       #'(list (list var k) ...)))))
(check (scalar-in-ellipsis (10 20) 99) => '((10 99) (20 99)))

;; A syntax-rules pattern scalar alongside a repeated group keeps its single
;; value too: k is bound once yet used inside the (var ...) repeats.
(define-syntax pattern-scalar-in-ellipsis
  (syntax-rules ()
    ((_ (var ...) k)
     (list (list var k) ...))))
(check (pattern-scalar-in-ellipsis (10 20) 'zz) => '((10 zz) (20 zz)))

;; The with-syntax shape srfi-165's computation-fn exposed: a generated
;; temporary repeats with the pattern group while the with-syntax scalar env
;; stays constant inside the repeated element.  Exercise the real macro:
(check
  (let ((cf-var (make-computation-environment-variable 'cfv 100 #f)))
    (computation-run
      (computation-with ((cf-var 999))
        (make-computation
          (lambda (compute)
            (compute (computation-fn ((v cf-var)) (computation-pure v))))))))
  => 999)

(check-report)
