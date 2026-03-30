(import (liii check)
        (liii iset)
) ;import

(check-set-mode! 'report-failed)

(define neg-set (list->iset (iota 20 -100 3)))

;;
;; iset-adjoin
;; 返回一个新集合，包含原集合的所有元素以及新增的元素。
;;
;; 语法
;; ----
;; (iset-adjoin iset element ...)
;;
;; 参数
;; ----
;; iset : iset
;; 初始集合。
;;
;; element ... : exact-integer
;; 要添加的元素。
;;
;; 返回值
;; -----
;; 返回一个新的 iset。
;;
(check (iset->list (iset-adjoin (iset 1 3 5) 0)) => '(0 1 3 5))
(check-true (iset-contains? (iset-adjoin neg-set 10) 10))

(check-report)
