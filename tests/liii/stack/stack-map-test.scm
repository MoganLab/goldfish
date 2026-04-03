(import (liii check)
        (liii stack)
) ;import

;; stack-map
;; 对栈中每个元素应用函数，返回新栈。
;;
;; 语法
;; ----
;; (stack-map proc s)
;;
;; 参数
;; ----
;; proc : procedure?
;; 应用到每个元素的函数，接受一个参数，返回新值
;;
;; s : stack?
;; 要映射的栈
;;
;; 返回值
;; ----
;; stack?
;; 包含映射后元素的新栈，原栈保持不变
;;
;; 说明
;; ----
;; stack-map 遍历栈中每个元素，将函数应用后结果组成新栈。
;; 元素顺序保持不变（从栈顶到栈底）。
;; 原栈不会被修改，返回的是新创建的栈。
;;
;; 错误处理
;; ----
;; type-error 当 proc 不是过程时
;; type-error 当 s 不是栈时

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

; Error handling tests
(check-catch 'type-error (stack-map "not-a-proc" (stack 1 2)))
(check-catch 'type-error (stack-map (lambda (x) x) 'not-a-stack))

(check-report)
