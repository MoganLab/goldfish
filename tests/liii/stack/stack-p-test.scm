(import (liii check)
        (liii stack)
) ;import

;; stack?
;; 检查对象是否为栈。
;;
;; 语法
;; ----
;; (stack? obj)
;;
;; 参数
;; ----
;; obj : any
;; 要检查的对象
;;
;; 返回值
;; ----
;; boolean
;; 如果对象是栈则返回 #t，否则返回 #f
;;
;; 说明
;; ----
;; stack? 是一个谓词函数，用于判断任意对象是否为栈类型。
;; 只有使用 make-stack、stack 或 list->stack 创建的对象返回 #t。
;; 列表、向量等其他集合类型返回 #f。

; Test stack? with stack
(let ((s (make-stack)))
  (check (stack? s) => #t)
) ;let

(let ((s (stack 1 2 3)))
  (check (stack? s) => #t)
) ;let

; Test stack? with non-stack values
(check (stack? '()) => #f)
(check (stack? '(1 2 3)) => #f)
(check (stack? 42) => #f)
(check (stack? "hello") => #f)
(check (stack? #t) => #f)
(check (stack? 'symbol) => #f)
(check (stack? (list 1 2 3)) => #f)

; Test that internal record is not exposed
(check (stack? (list->stack '(1 2 3))) => #t)

(check-report)
