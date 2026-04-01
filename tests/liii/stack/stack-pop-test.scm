(import (liii check)
        (liii stack)
) ;import

; Test stack-pop! returns top element
(let ((s (stack 1 2 3)))
  (check (stack-pop! s) => 1)
  (check (stack-pop! s) => 2)
  (check (stack-pop! s) => 3)
) ;let

; Test stack-pop! modifies stack
(let ((s (stack 1 2 3)))
  (check (stack-size s) => 3)
  (stack-pop! s)
  (check (stack-size s) => 2)
  (check (stack-top s) => 2)
) ;let

; Test stack-pop! all elements
(let ((s (stack 1 2)))
  (stack-pop! s)
  (stack-pop! s)
  (check (stack-empty? s) => #t)
) ;let

; Test stack-push! then stack-pop!
(let ((s (make-stack)))
  (stack-push! s 1)
  (stack-push! s 2)
  (check (stack-pop! s) => 2)
  (check (stack-pop! s) => 1)
  (check (stack-empty? s) => #t)
) ;let

; Test stack-pop! with LIFO order
(let ((s (make-stack)))
  (stack-push! s 'first)
  (stack-push! s 'second)
  (stack-push! s 'third)
  (check (stack-pop! s) => 'third)
  (check (stack-pop! s) => 'second)
  (check (stack-pop! s) => 'first)
) ;let

(check-report)
