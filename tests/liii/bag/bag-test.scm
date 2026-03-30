(import (liii check)
        (liii bag)
        (liii error))

(check-set-mode! 'report-failed)

;; Data Setup
(define b-empty (bag))
(define comp (bag-comparator b-empty))

;; bag 函数测试
;;
;; 语法
;; ----
;; (bag element ...)
;;
;; 参数
;; ----
;; element ... : any
;; 初始元素。
;;
;; 返回值
;; -----
;; 返回包含指定元素的 bag。

(define b-1-2 (bag 1 2 2))
(define b-list (bag->list b-1-2))
(check (bag-member b-1-2 1 #f) => 1)
(check (bag-member b-1-2 2 #f) => 2)
(check (bag-member b-1-2 3 'none) => 'none)
(check-true (eq? (bag-comparator b-1-2) comp))
(check (bag-member b-empty 1 'missing) => 'missing)

;; bag->list should include duplicates
(check-false (not (member 1 b-list)))
(check-false (not (member 2 b-list)))
(check (length b-list) => 3)

;; 不同类型元素也可存入 bag
(define b-mixed (bag "a" 'a 0))
(check (bag-member b-mixed "a" #f) => "a")
(check (bag-member b-mixed 'a #f) => 'a)
(check (bag-member b-mixed 0 #f) => 0)

;; equal? 等价元素应命中
(define a1 "hello")
(define a2 (string-copy a1))
(define b-strings (bag a1))
(check-true (string=? (bag-member b-strings a2 #f) "hello"))

(check-report)
