(import (liii check))

;; Bug6: internal defines with define-values + forward reference
(check (let ()
         (define-values (t1 t2) (values 1 2))
         (define a t1)
         (+ a 1))
       => 2)

;; variant: define-values then plain define, same order
(check (let ()
         (define x 10)
         (define y (+ x 1))
         (+ x y))
       => 21)

;; variant: let* body internal defines
(check (let ((base 5))
         (define z (+ base 1))
         (define w (* z 2))
         (+ z w))
       => 18)

(check-report)
