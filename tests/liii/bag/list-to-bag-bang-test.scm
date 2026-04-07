(import (liii check)
        (liii bag)
        (liii error)
) ;import

(check-set-mode! 'report-failed)

;; list->bag! 函数测试
;;
;; 语法
;; ----
;; (list->bag! bag list)
;;
;; 参数
;; ----
;; bag : bag
;; 目标 bag。
;;
;; list : list
;; 要并入的元素列表。
;;
;; 返回值
;; ------
;; 返回修改后的 bag（与传入的 bag 是同一个对象）。

(define b-list-merge (bag 1 2))
(define b-list-merge-result (list->bag! b-list-merge '(2 3 3)))
(check-true (eq? b-list-merge-result b-list-merge))
(check (bag-size b-list-merge) => 5)
(check (bag-count (lambda (x) (= x 2)) b-list-merge) => 2)
(check (bag-count (lambda (x) (= x 3)) b-list-merge) => 2)
(list->bag! b-list-merge '())
(check (bag-size b-list-merge) => 5)
(check-catch 'type-error (list->bag! "not a bag" '(1 2)))

(check-report)
