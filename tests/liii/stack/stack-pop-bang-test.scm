(import (liii check) (liii stack))


;; stack-pop!
;; 从栈顶弹出元素。
;;
;; 语法
;; ----
;; (stack-pop! stack)
;;
;; 参数
;; ----
;; stack : stack?
;; 要操作的栈，必须非空
;;
;; 返回值
;; ----
;; any
;; 返回被弹出的栈顶元素
;;
;; 说明
;; ----
;; stack-pop! 是栈的核心修改操作，移除并返回栈顶元素。
;; 该操作会修改原栈，时间复杂度为 O(1)。
;; 弹出后，下一个元素成为新的栈顶。
;; 对空栈执行 pop 操作会导致错误。
;;
;; 错误处理
;; ----
;; type-error 当参数不是栈时
;; stack-pop! 当栈为空时


(let ((s (stack 1 2 3)))
  (check (stack-pop! s) => 1)
  (check (stack-pop! s) => 2)
  (check (stack-pop! s) => 3)
) ;let


(let ((s (stack 1 2 3)))
  (check (stack-size s) => 3)
  (stack-pop! s)
  (check (stack-size s) => 2)
  (check (stack-top s) => 2)
) ;let


(let ((s (stack 1 2)))
  (stack-pop! s)
  (stack-pop! s)
  (check (stack-empty? s) => #t)
) ;let


(let ((s (make-stack)))
  (stack-push! s 1)
  (stack-push! s 2)
  (check (stack-pop! s) => 2)
  (check (stack-pop! s) => 1)
  (check (stack-empty? s) => #t)
) ;let


(let ((s (make-stack)))
  (stack-push! s 'first)
  (stack-push! s 'second)
  (stack-push! s 'third)
  (check (stack-pop! s) => 'third)
  (check (stack-pop! s) => 'second)
  (check (stack-pop! s) => 'first)
) ;let


(check-catch 'type-error
  (stack-pop! 'not-a-stack)
) ;check-catch


(check-catch 'value-error
  (stack-pop! (make-stack))
) ;check-catch


(check-report)
