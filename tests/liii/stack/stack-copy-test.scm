(import (liii check)
        (liii stack)
) ;import

; Test stack-copy with empty stack
(let ((s (make-stack)))
  (let ((copy (stack-copy s)))
    (check (stack? copy) => #t)
    (check (stack-empty? copy) => #t)
  ) ;let
) ;let

; Test stack-copy with single element
(let ((s (stack 1)))
  (let ((copy (stack-copy s)))
    (check (stack-top copy) => 1)
  ) ;let
) ;let

; Test stack-copy with multiple elements
(let ((s (stack 1 2 3)))
  (let ((copy (stack-copy s)))
    (check (stack->list copy) => '(1 2 3))
  ) ;let
) ;let

; Test stack-copy creates independent copy
(let ((s (stack 1 2 3)))
  (let ((copy (stack-copy s)))
    (stack-push! copy 0)
    ; Original should be unchanged
    (check (stack->list s) => '(1 2 3))
    (check (stack->list copy) => '(0 1 2 3))
  ) ;let
) ;let

; Test stack-copy after pop
(let ((s (stack 1 2 3)))
  (stack-pop! s)
  (let ((copy (stack-copy s)))
    (check (stack->list copy) => '(2 3))
  ) ;let
) ;let

; Test stack-copy preserves order
(let ((s (make-stack)))
  (stack-push! s 'first)
  (stack-push! s 'second)
  (let ((copy (stack-copy s)))
    (check (stack-pop! copy) => 'second)
    (check (stack-pop! copy) => 'first)
  ) ;let
) ;let

(check-report)
