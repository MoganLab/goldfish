(import (liii check)
        (liii stack)
) ;import

; Test stack-for-each on empty stack
(let ((s (make-stack))
      (count 0))
  (stack-for-each (lambda (x) (set! count (+ count 1)))
s)
  (check count => 0))

; Test stack-for-each on single element
(let ((s (stack 1))
      (sum 0))
  (stack-for-each (lambda (x) (set! sum (+ sum x))) s)
  (check sum => 1)
) ;let

; Test stack-for-each on multiple elements
(let ((s (stack 1 2 3))
      (sum 0))
  (stack-for-each (lambda (x) (set! sum (+ sum x))) s)
  (check sum => 6)
) ;let

; Test stack-for-each processes elements in order
(let ((s (stack 1 2 3))
      (lst '()))
  (stack-for-each (lambda (x) (set! lst (cons x lst))) s)
  ; Elements are processed from top to bottom
  (check lst => '(3 2 1))
) ;let

; Test stack-for-each doesn't modify stack
(let ((s (stack 1 2 3)))
  (stack-for-each (lambda (x) (* x 2)) s)
  (check (stack->list s) => '(1 2 3))
) ;let

; Test stack-for-each with side effects
(let ((s (stack "a" "b" "c"))
      (result ""))
  (stack-for-each (lambda (x) (set! result (string-append result x))) s)
  (check result => "abc")
) ;let

(check-report)
