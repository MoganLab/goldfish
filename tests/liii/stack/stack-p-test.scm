(import (liii check)
        (liii stack)
) ;import

; Test stack? with stack
(let ((s (make-stack)))
  (check (stack? s) => #t)
) ;let

(let ((s (stack 1 2 3)))
  (check (stack? s) => #t)
) ;let

; Test stack? with non-stack values
(check (stack? '()) => #f)
(check (stack? '(1 2 3)) => #f)
(check (stack? 42) => #f)
(check (stack? "hello") => #f)
(check (stack? #t) => #f)
(check (stack? 'symbol) => #f)
(check (stack? (list 1 2 3)) => #f)

; Test that internal record is not exposed
(check (stack? (list->stack '(1 2 3))) => #t)

(check-report)
