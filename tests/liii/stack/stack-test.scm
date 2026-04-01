(import (liii check)
        (liii stack)
) ;import

; Test stack with no arguments
(let ((s (stack)))
  (check (stack? s) => #t)
  (check (stack-empty? s) => #t)
  (check (stack-size s) => 0)
) ;let

; Test stack with single element
(let ((s (stack 1)))
  (check (stack? s) => #t)
  (check (stack-empty? s) => #f)
  (check (stack-size s) => 1)
  (check (stack-top s) => 1)
) ;let

; Test stack with multiple elements
(let ((s (stack 1 2 3)))
  (check (stack? s) => #t)
  (check (stack-size s) => 3)
  ; Top should be the first element (1)
  (check (stack-top s) => 1)
) ;let

; Test stack with different types
(let ((s (stack 'a "hello" 42)))
  (check (stack-size s) => 3)
  (check (stack-top s) => 'a)
) ;let

(check-report)
