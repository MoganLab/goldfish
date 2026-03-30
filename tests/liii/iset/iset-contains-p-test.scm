(import (liii check)
        (liii iset)
        (only (srfi srfi-1) every any)
) ;import

(check-set-mode! 'report-failed)

(define pos-seq (iota 20 100 3))
(define neg-seq (iota 20 -100 3))
(define pos-set (list->iset pos-seq))
(define neg-set (list->iset neg-seq))

;;
;; iset-contains?
;; 检查集合是否包含指定元素。
;;
;; 语法
;; ----
;; (iset-contains? iset element)
;;
;; 参数
;; ----
;; iset : iset
;; 目标集合。
;;
;; element : exact-integer
;; 要检查的元素。
;;
;; 返回值
;; -----
;; 如果 iset 包含 element，返回 #t；否则返回 #f。
;;
(check-true (iset-contains? (iset 2 3 5 7 11) 5))
(check-false (iset-contains? (iset 2 3 5 7 11) 4))
(check-true (every (lambda (n) (iset-contains? pos-set n)) pos-seq))
(check-false (any (lambda (n) (iset-contains? pos-set n)) neg-seq))

(check-report)
