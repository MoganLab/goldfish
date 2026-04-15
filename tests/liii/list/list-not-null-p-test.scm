(import (liii list) (liii check))


(check-set-mode! 'report-failed)


;; list-not-null? 函数测试
;;
;; 语法
;; ----
;; (list-not-null? obj)
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
;; list-not-null?是not-null-list?的别名，检查对象是否为非空列表。
;;
;; 示例
;; ----
;; (list-not-null? (list 1)) => #t
;; (list-not-null? (list 1 2 3)) => #t
;; (list-not-null? ()) => #f
;; (list-not-null? '(a . b)) => #f
;; (list-not-null? 1) => #f


(check (list-not-null? (list 1)) => #t)
(check (list-not-null? (list 1 2 3))
  =>
  #t
) ;check
(check (list-not-null? '(a)) => #t)
(check (list-not-null? '(a b c)) => #t)
(check (list-not-null? ()) => #f)
(check (list-not-null? '(a . b)) => #f)
(check (list-not-null? 1) => #f)


(check-report)
