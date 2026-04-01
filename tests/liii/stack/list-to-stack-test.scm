(import (liii check)
        (liii stack)
) ;import

; Test list->stack with empty list
(let ((s (list->stack '())))
  (check (stack? s) => #t)
  (check (stack-empty? s) => #t)
  (check (stack-size s) => 0)
) ;let

; Test list->stack with single element
(let ((s (list->stack '(1))))
  (check (stack? s) => #t)
  (check (stack-size s) => 1)
  (check (stack-top s) => 1)
) ;let

; Test list->stack with multiple elements
; First element of list becomes top of stack
(let ((s (list->stack '(1 2 3))))
  (check (stack-size s) => 3)
  (check (stack-top s) => 1)
) ;let

; Test list->stack roundtrip with stack->list
(let ((lst '(1 2 3 4 5)))
  (let ((s (list->stack lst)))
    (check (stack->list s) => lst)
  ) ;let
) ;let

; Test list->stack then stack-pop!
(let ((s (list->stack '(a b c))))
  (check (stack-pop! s) => 'a)
  (check (stack-pop! s) => 'b)
  (check (stack-pop! s) => 'c)
) ;let

(check-report)
