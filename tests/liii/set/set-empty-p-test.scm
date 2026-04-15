(import (liii check)
  (liii error)
  (liii set)
) ;import


(check-set-mode! 'report-failed)


;; set-empty?
;; 检查 set 是否为空。
;;
;; 语法
;; ----
;; (set-empty? set)
;;
;; 参数
;; ----
;; set : set
;; 要检查的 set。
;;
;; 返回值
;; ----
;; boolean
;; 如果 set 为空，返回 #t；否则返回 #f。
;;
;; 示例
;; ----
;; (set-empty? (set)) => #t
;; (set-empty? (set 1)) => #f
;;
;; 错误处理
;; ----
;; type-error
;; 当 set 参数不是 set 时抛出。


(define s-empty (set))
(define s-1 (set 1))


(check-true (set-empty? s-empty))
(check-false (set-empty? s-1))
(check-catch 'type-error
  (set-empty? "not a set")
) ;check-catch


(check-report)
