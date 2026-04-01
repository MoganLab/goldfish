(import (liii check)
        (liii stack)
) ;import

; Test stack-map! on empty stack
(let ((s (make-stack)))
  (stack-map! (lambda (x) (* x 2)) s)
  (check (stack-empty? s) => #t)
) ;let

; Test stack-map! on single element
(let ((s (stack 1)))
  (stack-map! (lambda (x) (* x 2)) s)
  (check (stack-top s) => 2)
) ;let

; Test stack-map! on multiple elements
(let ((s (stack 1 2 3)))
  (stack-map! (lambda (x) (* x 2)) s)
  (check (stack->list s) => '(2 4 6))
) ;let

; Test stack-map! modifies original stack
(let ((s (stack 1 2 3)))
  (stack-map! (lambda (x) (* x 2)) s)
  (check (stack->list s) => '(2 4 6))
) ;let

; Test stack-map! returns the stack
(let ((s (stack 1 2 3)))
  (check (stack-map! (lambda (x) (* x 2)) s) => s)
) ;let

; Test stack-map! with different function
(let ((s (stack 10 20 30)))
  (stack-map! (lambda (x) (/ x 10)) s)
  (check (stack->list s) => '(1 2 3))
) ;let

(check-report)
