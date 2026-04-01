(import (liii check)
        (liii stack)
) ;import

; Test stack-size with empty stack
(let ((s (make-stack)))
  (check (stack-size s) => 0)
) ;let

; Test stack-size with single element
(let ((s (stack 1)))
  (check (stack-size s) => 1)
) ;let

; Test stack-size with multiple elements
(let ((s (stack 1 2 3)))
  (check (stack-size s) => 3)
) ;let

(let ((s (stack 'a 'b 'c 'd 'e)))
  (check (stack-size s) => 5)
) ;let

; Test stack-size after push
(let ((s (make-stack)))
  (check (stack-size s) => 0)
  (stack-push! s 1)
  (check (stack-size s) => 1)
  (stack-push! s 2)
  (check (stack-size s) => 2)
) ;let

; Test stack-size after pop
(let ((s (stack 1 2 3)))
  (check (stack-size s) => 3)
  (stack-pop! s)
  (check (stack-size s) => 2)
  (stack-pop! s)
  (check (stack-size s) => 1)
) ;let

(check-report)
