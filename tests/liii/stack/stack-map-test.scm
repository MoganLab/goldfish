(import (liii check)
        (liii stack)
) ;import

; Test stack-map on empty stack
(let ((s (make-stack)))
  (let ((result (stack-map (lambda (x) (* x 2)) s)))
    (check (stack? result) => #t)
    (check (stack-empty? result) => #t)
  ) ;let
) ;let

; Test stack-map on single element
(let ((s (stack 1)))
  (let ((result (stack-map (lambda (x) (* x 2)) s)))
    (check (stack-top result) => 2)
  ) ;let
) ;let

; Test stack-map on multiple elements
(let ((s (stack 1 2 3)))
  (let ((result (stack-map (lambda (x) (* x 2)) s)))
    (check (stack->list result) => '(2 4 6))
  ) ;let
) ;let

; Test stack-map doesn't modify original stack
(let ((s (stack 1 2 3)))
  (stack-map (lambda (x) (* x 2)) s)
  (check (stack->list s) => '(1 2 3))
) ;let

; Test stack-map with different function
(let ((s (stack 1 2 3)))
  (let ((result (stack-map (lambda (x) (+ x 10)) s)))
    (check (stack->list result) => '(11 12 13))
  ) ;let
) ;let

; Test stack-map with list result function
(let ((s (stack 1 2)))
  (let ((result (stack-map (lambda (x) (list x x)) s)))
    (check (stack->list result) => '((1 1) (2 2)))
  ) ;let
) ;let

(check-report)
