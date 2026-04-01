(import (liii check)
        (liii stack)
) ;import

; Test stack-empty? with empty stack
(let ((s (make-stack)))
  (check (stack-empty? s) => #t)
) ;let

(let ((s (stack)))
  (check (stack-empty? s) => #t)
) ;let

; Test stack-empty? with non-empty stack
(let ((s (make-stack '(1))))
  (check (stack-empty? s) => #f)
) ;let

(let ((s (stack 1)))
  (check (stack-empty? s) => #f)
) ;let

(let ((s (stack 1 2 3)))
  (check (stack-empty? s) => #f)
) ;let

; Test stack-empty? after operations
(let ((s (stack 1 2)))
  (check (stack-empty? s) => #f)
  (stack-pop! s)
  (check (stack-empty? s) => #f)
  (stack-pop! s)
  (check (stack-empty? s) => #t)
) ;let

; Test stack-empty? after push
(let ((s (make-stack)))
  (check (stack-empty? s) => #t)
  (stack-push! s 1)
  (check (stack-empty? s) => #f)
) ;let

(check-report)
