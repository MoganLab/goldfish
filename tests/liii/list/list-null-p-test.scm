(import (liii list)
        (liii check)
) ;import

(check-set-mode! 'report-failed)

;; list-null? 函数测试
;;
;; 语法
;; ----
;; (list-null? obj)
;;
;; 参数
;; ----
;; obj : any
;; 任意对象。
;;
;; 返回值
;; ------
;; boolean?
;; 如果obj是空列表，返回#t，否则返回#f。
;;
;; 示例
;; ----
;; (list-null? (list 1)) => #f
;; (list-null? (list 1 2 3)) => #f
;; (list-null? ()) => #t
;; (list-null? '(a . b)) => #f
;; (list-null? 1) => #f

(check (list-null? (list 1)) => #f)
(check (list-null? (list 1 2 3)) => #f)
(check (list-null? '(a)) => #f)
(check (list-null? '(a b c)) => #f)
(check (list-null? ()) => #t)
(check (list-null? '(a . b)) => #f)
(check (list-null? 1) => #f)

(check-report)
