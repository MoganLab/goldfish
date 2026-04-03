(import (liii check)
        (liii stack)
) ;import

;; stack-empty?
;; 检查栈是否为空。
;;
;; 语法
;; ----
;; (stack-empty? s)
;;
;; 参数
;; ----
;; s : stack?
;; 要检查的栈
;;
;; 返回值
;; ----
;; boolean
;; 如果栈为空则返回 #t，否则返回 #f
;;
;; 说明
;; ----
;; 空栈是指不包含任何元素的栈。
;; 对空栈执行 stack-pop! 或 stack-top 操作会导致错误。
;;
;; 错误处理
;; ----
;; type-error 当参数不是栈时

; Test stack-empty? with empty stack
(let ((s (make-stack)))
  (check (stack-empty? s) => #t)
) ;let

(let ((s (stack)))
  (check (stack-empty? s) => #t)
) ;let

; Test stack-empty? with non-empty stack
(let ((s (make-stack '(1))))
  (check (stack-empty? s) => #f)
) ;let

(let ((s (stack 1)))
  (check (stack-empty? s) => #f)
) ;let

(let ((s (stack 1 2 3)))
  (check (stack-empty? s) => #f)
) ;let

; Test stack-empty? after operations
(let ((s (stack 1 2)))
  (check (stack-empty? s) => #f)
  (stack-pop! s)
  (check (stack-empty? s) => #f)
  (stack-pop! s)
  (check (stack-empty? s) => #t)
) ;let

; Test stack-empty? after push
(let ((s (make-stack)))
  (check (stack-empty? s) => #t)
  (stack-push! s 1)
  (check (stack-empty? s) => #f)
) ;let

(check-report)
