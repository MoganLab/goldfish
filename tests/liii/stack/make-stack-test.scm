(import (liii check) (liii stack))


;; make-stack
;; 创建一个新的栈。
;;
;; 语法
;; ----
;; (make-stack)
;; (make-stack lst)
;;
;; 参数
;; ----
;; lst : list? 可选
;; 用于初始化栈的元素列表，列表的第一个元素将成为栈顶
;;
;; 返回值
;; ----
;; stack?
;; 新创建的栈
;;
;; 说明
;; ----
;; 无参数调用时创建一个空栈。
;; 传入列表时，列表的第一个元素成为栈顶，保持列表原有顺序。
;;
;; 错误处理
;; ----
;; type-error 当传入的参数不是列表时


(let ((s (make-stack)))
  (check (stack? s) => #t)
  (check (stack-empty? s) => #t)
  (check (stack-size s) => 0)
) ;let


(let ((s (make-stack '(1 2 3))))
  (check (stack? s) => #t)
  (check (stack-empty? s) => #f)
  (check (stack-size s) => 3)
  (check (stack-top s) => 1)
) ;let


(let ((s (make-stack '())))
  (check (stack? s) => #t)
  (check (stack-empty? s) => #t)
  (check (stack-size s) => 0)
) ;let


(check-report)
