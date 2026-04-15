(import (liii check) (liii stack))


;; stack-copy
;; 复制栈。
;;
;; 语法
;; ----
;; (stack-copy s)
;;
;; 参数
;; ----
;; s : stack?
;; 要复制的栈
;;
;; 返回值
;; ----
;; stack?
;; 原栈的浅拷贝，修改新栈不会影响原栈
;;
;; 说明
;; ----
;; stack-copy 创建一个新的栈，包含与原栈相同的元素。
;; 返回的栈是独立的，对其修改不会影响原栈。
;; 注意这是浅拷贝，栈中的元素本身不会被复制。
;;
;; 错误处理
;; ----
;; type-error 当参数不是栈时


(let ((s (make-stack)))
  (let ((copy (stack-copy s)))
    (check (stack? copy) => #t)
    (check (stack-empty? copy) => #t)
  ) ;let
) ;let


(let ((s (stack 1)))
  (let ((copy (stack-copy s)))
    (check (stack-top copy) => 1)
  ) ;let
) ;let


(let ((s (stack 1 2 3)))
  (let ((copy (stack-copy s)))
    (check (stack->list copy) => '(1 2 3))
  ) ;let
) ;let


(let ((s (stack 1 2 3)))
  (let ((copy (stack-copy s)))
    (stack-push! copy 0)
    (check (stack->list s) => '(1 2 3))
    (check (stack->list copy) => '(0 1 2 3))
  ) ;let
) ;let


(let ((s (stack 1 2 3)))
  (stack-pop! s)
  (let ((copy (stack-copy s)))
    (check (stack->list copy) => '(2 3))
  ) ;let
) ;let


(let ((s (make-stack)))
  (stack-push! s 'first)
  (stack-push! s 'second)
  (let ((copy (stack-copy s)))
    (check (stack-pop! copy) => 'second)
    (check (stack-pop! copy) => 'first)
  ) ;let
) ;let


(check-catch 'type-error
  (stack-copy 'not-a-stack)
) ;check-catch


(check-report)
