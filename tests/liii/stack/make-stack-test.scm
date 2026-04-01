(import (liii check)
        (liii stack)
) ;import

; Test make-stack with no arguments
(let ((s (make-stack)))
  (check (stack? s) => #t)
  (check (stack-empty? s) => #t)
  (check (stack-size s) => 0)
) ;let

; Test make-stack with a list
(let ((s (make-stack '(1 2 3))))
  (check (stack? s) => #t)
  (check (stack-empty? s) => #f)
  (check (stack-size s) => 3)
  (check (stack-top s) => 1)
) ;let

; Test make-stack with empty list
(let ((s (make-stack '())))
  (check (stack? s) => #t)
  (check (stack-empty? s) => #t)
  (check (stack-size s) => 0)
) ;let

(check-report)
