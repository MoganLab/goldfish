(import (liii check)
        (liii stack)
) ;import

;; stack-for-each
;; 遍历栈中每个元素，执行副作用操作。
;;
;; 语法
;; ----
;; (stack-for-each proc s)
;;
;; 参数
;; ----
;; proc : procedure?
;; 应用到每个元素的函数，接受一个参数，返回值被忽略
;;
;; s : stack?
;; 要遍历的栈
;;
;; 返回值
;; ----
;; 未定义
;; 此函数用于副作用，不返回有意义的值
;;
;; 说明
;; ----
;; stack-for-each 从栈顶到栈底依次遍历每个元素。
;; 主要用于执行副作用操作（如打印、累计等）。
;; 不会修改栈本身。
;;
;; 错误处理
;; ----
;; type-error 当 proc 不是过程时
;; type-error 当 s 不是栈时

; Test stack-for-each on empty stack
(let ((s (make-stack))
      (count 0))
  (stack-for-each (lambda (x) (set! count (+ count 1))) s)
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

; Error handling tests
(check-catch 'type-error (stack-for-each "not-a-proc" (stack 1 2)))
(check-catch 'type-error (stack-for-each (lambda (x) x) 'not-a-stack))

(check-report)
