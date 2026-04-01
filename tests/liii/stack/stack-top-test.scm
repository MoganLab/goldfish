(import (liii check)
        (liii stack)
) ;import

; Test stack-top with single element
(let ((s (stack 1)))
  (check (stack-top s) => 1)
) ;let

; Test stack-top with multiple elements (top is first)
(let ((s (stack 1 2 3)))
  (check (stack-top s) => 1)
) ;let

; Test stack-top with different types
(let ((s (stack 'symbol "string" 42)))
  (check (stack-top s) => 'symbol)
) ;let

; Test stack-top doesn't modify stack
(let ((s (stack 1 2 3)))
  (check (stack-top s) => 1)
  (check (stack-size s) => 3)
  (check (stack-top s) => 1)
) ;let

; Test stack-top with nested list
(let ((s (stack '(1 2) '(3 4))))
  (check (stack-top s) => '(1 2))
) ;let

(check-report)
