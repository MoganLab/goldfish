(import (liii check)
        (liii error)
        (liii set)
) ;import

(check-set-mode! 'report-failed)

;; set-contains?
;; 检查 set 是否包含指定元素。
;;
;; 语法
;; ----
;; (set-contains? set element)
;;
;; 参数
;; ----
;; set : set
;; 目标 set。
;;
;; element : any
;; 要检查的元素。
;;
;; 返回值
;; ----
;; boolean
;; 如果 set 包含 element，返回 #t；否则返回 #f。
;;
;; 示例
;; ----
;; (set-contains? (set 1) 1) => #t
;; (set-contains? (set 1) 2) => #f
;;
;; 错误处理
;; ----
;; type-error
;; 当 set 参数不是 set 时抛出。

(define s-empty (set))
(define s-1 (set 1))

(check-true (set-contains? s-1 1))
(check-false (set-contains? s-1 2))
(check-false (set-contains? s-empty 1))
(check-catch 'type-error (set-contains? "not a set" 1))

(check-report)
