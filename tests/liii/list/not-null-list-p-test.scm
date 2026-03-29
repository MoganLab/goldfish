(import (liii list)
        (liii check)
) ;import

(check-set-mode! 'report-failed)

;; not-null-list? 函数测试
;;
;; 语法
;; ----
;; (not-null-list? obj)
;;
;; 参数
;; ----
;; obj : any
;; 任意对象。
;;
;; 返回值
;; ------
;; boolean?
;; 如果obj是非空列表，返回#t，否则返回#f。
;;
;; 说明
;; ----
;; not-null-list?检查对象是否为非空列表。
;;
;; 示例
;; ----
;; (not-null-list? (list 1)) => #t
;; (not-null-list? (list 1 2 3)) => #t
;; (not-null-list? ()) => #f
;; (not-null-list? '(a . b)) => #f
;; (not-null-list? 1) 抛出 type-error

(check (not-null-list? (list 1)) => #t)
(check (not-null-list? (list 1 2 3)) => #t)
(check (not-null-list? '(a)) => #t)
(check (not-null-list? '(a b c)) => #t)
(check (not-null-list? ()) => #f)

; '(a) is a pair and a list
; '(a . b) is a pair but not a list
(check (not-null-list? '(a . b)) => #f)

(check-catch 'type-error (not-null-list? 1))

(check-report)
