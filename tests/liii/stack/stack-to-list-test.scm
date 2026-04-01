(import (liii check)
        (liii stack)
) ;import

; Test stack->list with empty stack
(let ((s (make-stack)))
  (check (stack->list s) => '())
) ;let

; Test stack->list with single element
(let ((s (stack 1)))
  (check (stack->list s) => '(1))
) ;let

; Test stack->list with multiple elements
; Elements should be returned as stored (top first)
(let ((s (stack 1 2 3)))
  (check (stack->list s) => '(1 2 3))
) ;let

; Test stack->list doesn't modify stack
(let ((s (stack 1 2 3)))
  (stack->list s)
  (check (stack-size s) => 3)
  (check (stack-top s) => 1)
) ;let

; Test stack->list after push/pop operations
(let ((s (make-stack)))
  (stack-push! s 'a)
  (stack-push! s 'b)
  (stack-push! s 'c)
  (check (stack->list s) => '(c b a))
  (stack-pop! s)
  (check (stack->list s) => '(b a))
) ;let

(check-report)
