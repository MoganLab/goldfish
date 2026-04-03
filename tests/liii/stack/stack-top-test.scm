(import (liii check)
        (liii stack)
) ;import

;; stack-top
;; 获取栈顶元素。
;;
;; 语法
;; ----
;; (stack-top s)
;;
;; 参数
;; ----
;; s : stack?
;; 要访问的栈，必须非空
;;
;; 返回值
;; ----
;; any
;; 栈顶元素（第一个元素）
;;
;; 说明
;; ----
;; stack-top 只返回栈顶元素，不会修改栈。
;; 第一个传入构造函数的参数是栈顶。
;; 对空栈执行此操作会导致错误。
;;
;; 错误处理
;; ----
;; type-error 当参数不是栈时
;; value-error 当栈为空时

; Test stack-top with single element
(let ((s (stack 1)))
  (check (stack-top s) => 1)
) ;let

; Test stack-top with multiple elements (top is first)
(let ((s (stack 1 2 3)))
  (check (stack-top s) => 1)
) ;let

; Test stack-top with different types
(let ((s (stack 'symbol "string" 42)))
  (check (stack-top s) => 'symbol)
) ;let

; Test stack-top doesn't modify stack
(let ((s (stack 1 2 3)))
  (check (stack-top s) => 1)
  (check (stack-size s) => 3)
  (check (stack-top s) => 1)
) ;let

; Test stack-top with nested list
(let ((s (stack '(1 2) '(3 4))))
  (check (stack-top s) => '(1 2))
) ;let

; Error handling tests
(check-catch 'type-error (stack-top 'not-a-stack))

(check-catch 'value-error (stack-top (make-stack)))

(check-report)
