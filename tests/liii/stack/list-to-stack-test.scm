(import (liii check)
        (liii stack)
) ;import

;; list->stack
;; 从列表创建栈。
;;
;; 语法
;; ----
;; (list->stack lst)
;;
;; 参数
;; ----
;; lst : list?
;; 用于初始化栈的列表，列表的第一个元素将成为栈顶
;;
;; 返回值
;; ----
;; stack?
;; 新创建的栈
;;
;; 说明
;; ----
;; list->stack 是 make-stack 的别名，功能完全相同。
;; 列表的第一个元素成为栈顶，保持列表原有顺序。
;; 与 stack->list 可以形成往返转换。
;;
;; 错误处理
;; ----
;; type-error 当参数不是列表时

; Test list->stack with empty list
(let ((s (list->stack '())))
  (check (stack? s) => #t)
  (check (stack-empty? s) => #t)
  (check (stack-size s) => 0)
) ;let

; Test list->stack with single element
(let ((s (list->stack '(1))))
  (check (stack? s) => #t)
  (check (stack-size s) => 1)
  (check (stack-top s) => 1)
) ;let

; Test list->stack with multiple elements
; First element of list becomes top of stack
(let ((s (list->stack '(1 2 3))))
  (check (stack-size s) => 3)
  (check (stack-top s) => 1)
) ;let

; Test list->stack roundtrip with stack->list
(let ((lst '(1 2 3 4 5)))
  (let ((s (list->stack lst)))
    (check (stack->list s) => lst)
  ) ;let
) ;let

; Test list->stack then stack-pop!
(let ((s (list->stack '(a b c))))
  (check (stack-pop! s) => 'a)
  (check (stack-pop! s) => 'b)
  (check (stack-pop! s) => 'c)
) ;let

; Error handling test
(check-catch 'type-error (list->stack 'not-a-list))
(check-catch 'type-error (list->stack 123))

(check-report)
