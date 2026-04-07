(import (liii check)
        (liii bag)
        (liii error)
) ;import

(check-set-mode! 'report-failed)

;; Data Setup
(define b-empty (bag))
(define comp (bag-comparator b-empty))

;; list->bag 函数测试
;;
;; 语法
;; ----
;; (list->bag list)
;;
;; 参数
;; ----
;; list : list
;; 要转换的列表。
;;
;; 返回值
;; -----
;; 返回包含列表中所有元素的 bag（使用默认比较器，重复元素保留）。

(define b-list-1 (list->bag '(1 2 2 3)))
(check-true (bag? b-list-1))
(check-true (eq? (bag-comparator b-list-1) comp))
(check (bag-size b-list-1) => 4)
(check (bag-count (lambda (x) (= x 2)) b-list-1) => 2)
(define b-list-empty (list->bag '()))
(check-true (bag-empty? b-list-empty))

(check-report)
