(import (liii check) (liii stack))


;; stack-push!
;; 将元素压入栈顶。
;;
;; 语法
;; ----
;; (stack-push! stack elem)
;;
;; 参数
;; ----
;; stack : stack?
;; 要操作的栈
;;
;; elem : any
;; 要压入栈顶的元素
;;
;; 返回值
;; ----
;; stack?
;; 返回修改后的栈（便于链式调用）
;;
;; 说明
;; ----
;; stack-push! 是栈的核心修改操作，将新元素添加到栈顶。
;; 该操作会修改原栈，时间复杂度为 O(1)。
;; 新元素成为栈顶，原有元素依次下移。
;;
;; 错误处理
;; ----
;; type-error 当第一个参数不是栈时


(let ((s (make-stack)))
  (stack-push! s 1)
  (check (stack-size s) => 1)
  (check (stack-top s) => 1)
) ;let


(let ((s (make-stack)))
  (stack-push! s 1)
  (stack-push! s 2)
  (stack-push! s 3)
  (check (stack-size s) => 3)
  (check (stack-top s) => 3)
) ;let


(let ((s (stack 1 2)))
  (stack-push! s 3)
  (check (stack-size s) => 3)
  (check (stack-top s) => 3)
) ;let


(let ((s (make-stack)))
  (check (stack-push! s 1) => s)
) ;let


(let ((s (make-stack)))
  (stack-push! s 1)
  (stack-push! s "hello")
  (stack-push! s 'symbol)
  (check (stack-top s) => 'symbol)
) ;let


(check-catch 'type-error
  (stack-push! 'not-a-stack 1)
) ;check-catch


(check-report)
