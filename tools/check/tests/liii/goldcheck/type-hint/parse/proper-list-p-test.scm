;; proper-list?
;; 判断给定对象是否为标准的真列表（以 null 结尾，非点对列表）。
;;
;; 语法
;; ----
;; (proper-list? x)
;;
;; 参数
;; ----
;; x : any  待检验的对象
;;
;; 返回值
;; ------
;; boolean
;; 为合法的真列表返回 #t，点对列表或非 pair/null 结构返回 #f。

(import (scheme base) (liii check) (liii goldcheck type-hint parse))

(check-set-mode! 'report-failed)

;; 1. 真列表
(check (proper-list? '()) => #t)
(check (proper-list? '(1 2 3)) => #t)
(check (proper-list? '(a (b c) d)) => #t)

;; 2. 非真列表（点对或非列表）
(check (proper-list? '(a . b)) => #f)
(check (proper-list? '(1 2 . 3)) => #f)
(check (proper-list? 42) => #f)
(check (proper-list? "str") => #f)

(check-report)
