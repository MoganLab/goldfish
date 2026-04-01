(import (liii check)
        (liii stack)
) ;import

; Test stack-push! on empty stack
(let ((s (make-stack)))
  (stack-push! s 1)
  (check (stack-size s) => 1)
  (check (stack-top s) => 1)
) ;let

; Test stack-push! multiple times
(let ((s (make-stack)))
  (stack-push! s 1)
  (stack-push! s 2)
  (stack-push! s 3)
  (check (stack-size s) => 3)
  ; Top should be the last pushed
  (check (stack-top s) => 3)
) ;let

; Test stack-push! on non-empty stack
(let ((s (stack 1 2)))
  (stack-push! s 3)
  (check (stack-size s) => 3)
  (check (stack-top s) => 3)
) ;let

; Test stack-push! returns the stack
(let ((s (make-stack)))
  (check (stack-push! s 1) => s)
) ;let

; Test stack-push! with different types
(let ((s (make-stack)))
  (stack-push! s 1)
  (stack-push! s "hello")
  (stack-push! s 'symbol)
  (check (stack-top s) => 'symbol)
) ;let

(check-report)
