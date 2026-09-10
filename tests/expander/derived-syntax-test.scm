(import (goldfish) (scheme base) (liii check))

;; Derived-syntax smoke: each core-adjacent form evaluates to its value.
;; Previously display-only (passed vacuously); now asserted.
(check (let* ([x 1]) (cond [(= x 1) (case 1 ((1) (and #t #t)))]
                           [else     #f]))
       => #t)
(check (when #t (do ((i 0 (+ i 1))) ((= i 2) (force (delay 42))))) => 42)
(check (+ 1 2) => 3)
(check (let ([a 1]
             [b 2])
         (if (and (> a 0) (or #t (> b 3))) (parameterize () 9) 0))
       => 9)
(check (let-values ([(x y) (values 1 2)]) (+ x y)) => 3)
(check (guard (e (#t 42)) (raise 1)) => 42)

(define-record-type pt (make-pt x) pt? (x pt-x))
(check (pt-x (make-pt 5)) => 5)
(check (pt? (make-pt 5)) => #t)
(check (catch #t (lambda () (syntax-error "boom")) (lambda args 'syntax-error-raised))
       => 'syntax-error-raised)

(check-report)
