(import (liii check) (liii stack))


;; stack
;; 使用指定元素创建一个新栈。
;;
;; 语法
;; ----
;; (stack elem ...)
;;
;; 参数
;; ----
;; elem ... : any 零个或多个
;; 用于初始化栈的元素，第一个参数将成为栈顶
;;
;; 返回值
;; ----
;; stack?
;; 新创建的栈
;;
;; 说明
;; ----
;; stack 是一个变参构造函数，可以直接传入任意数量的元素。
;; 第一个传入的元素成为栈顶，与 make-stack 的列表参数行为一致。
;; 无参数调用时创建一个空栈。


(let ((s (stack)))
  (check (stack? s) => #t)
  (check (stack-empty? s) => #t)
  (check (stack-size s) => 0)
) ;let


(let ((s (stack 1)))
  (check (stack? s) => #t)
  (check (stack-empty? s) => #f)
  (check (stack-size s) => 1)
  (check (stack-top s) => 1)
) ;let


(let ((s (stack 1 2 3)))
  (check (stack? s) => #t)
  (check (stack-size s) => 3)
  (check (stack-top s) => 1)
) ;let


(let ((s (stack 'a "hello" 42)))
  (check (stack-size s) => 3)
  (check (stack-top s) => 'a)
) ;let


(check-report)
