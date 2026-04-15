(import (liii check) (liii set))


(check-set-mode! 'report-failed)


;; set?
;; 检查对象是否为 set。
;;
;; 语法
;; ----
;; (set? obj)
;;
;; 参数
;; ----
;; obj : any
;; 要检查的对象。
;;
;; 返回值
;; ----
;; boolean
;; 如果 obj 是 set，返回 #t；否则返回 #f。
;;
;; 示例
;; ----
;; (set? (set)) => #t
;; (set? "not a set") => #f
;;
;; 错误处理
;; ----
;; 无异常抛出


(define s-empty (set))
(define s-1 (set 1))


(check-true (set? s-empty))
(check-true (set? s-1))
(check-false (set? "not a set"))
(check-false (set? '()))
(check-false (set? #(1 2 3)))


(check-report)
