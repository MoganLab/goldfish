(import (liii check) (liii stack))


;; stack-size
;; 获取栈中元素个数。
;;
;; 语法
;; ----
;; (stack-size s)
;;
;; 参数
;; ----
;; s : stack?
;; 要查询的栈
;;
;; 返回值
;; ----
;; integer?
;; 栈中元素的数量，空栈返回 0
;;
;; 说明
;; ----
;; stack-size 返回栈中当前元素的数量。
;; 时间复杂度为 O(n)，因为需要遍历计算。
;;
;; 错误处理
;; ----
;; type-error 当参数不是栈时


(let ((s (make-stack)))
  (check (stack-size s) => 0)
) ;let


(let ((s (stack 1)))
  (check (stack-size s) => 1)
) ;let


(let ((s (stack 1 2 3)))
  (check (stack-size s) => 3)
) ;let


(let ((s (stack 'a 'b 'c 'd 'e)))
  (check (stack-size s) => 5)
) ;let


(let ((s (make-stack)))
  (check (stack-size s) => 0)
  (stack-push! s 1)
  (check (stack-size s) => 1)
  (stack-push! s 2)
  (check (stack-size s) => 2)
) ;let


(let ((s (stack 1 2 3)))
  (check (stack-size s) => 3)
  (stack-pop! s)
  (check (stack-size s) => 2)
  (stack-pop! s)
  (check (stack-size s) => 1)
) ;let


(check-catch 'type-error
  (stack-size 'not-a-stack)
) ;check-catch


(check-report)
