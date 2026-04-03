(import (liii check)
        (liii stack)
) ;import

;; stack->list
;; 将栈转换为列表。
;;
;; 语法
;; ----
;; (stack->list s)
;;
;; 参数
;; ----
;; s : stack?
;; 要转换的栈
;;
;; 返回值
;; ----
;; list?
;; 包含栈中所有元素的列表，栈顶为列表的第一个元素
;;
;; 说明
;; ----
;; stack->list 返回栈中元素的列表表示，不修改原栈。
;; 返回的列表顺序与栈一致：列表的第一个元素是栈顶。
;; 使用 list->stack 和 stack->list 可以进行往返转换。
;;
;; 错误处理
;; ----
;; type-error 当参数不是栈时

; Test stack->list with empty stack
(let ((s (make-stack)))
  (check (stack->list s) => '())
) ;let

; Test stack->list with single element
(let ((s (stack 1)))
  (check (stack->list s) => '(1))
) ;let

; Test stack->list with multiple elements
; Elements should be returned as stored (top first)
(let ((s (stack 1 2 3)))
  (check (stack->list s) => '(1 2 3))
) ;let

; Test stack->list doesn't modify stack
(let ((s (stack 1 2 3)))
  (stack->list s)
  (check (stack-size s) => 3)
  (check (stack-top s) => 1)
) ;let

; Test stack->list after push/pop operations
(let ((s (make-stack)))
  (stack-push! s 'a)
  (stack-push! s 'b)
  (stack-push! s 'c)
  (check (stack->list s) => '(c b a))
  (stack-pop! s)
  (check (stack->list s) => '(b a))
) ;let

; Error handling test
(check-catch 'type-error (stack->list 'not-a-stack))

(check-report)
