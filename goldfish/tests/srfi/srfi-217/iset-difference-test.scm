;;
;; iset-difference
;; 返回第一个集合与其余集合的差集。
;;

;; 语法
;; ----
;; (iset-difference iset1 iset2 ...)
;;

;; 参数
;; ----
;; set1 : iset
;; 第一个集合。
;;
;; set2 : iset
;; 第二个集合。

;; 返回值
;; ----
;; iset
;; 返回 set1 中不属于 set2 的元素组成的新 iset。

(import (liii check) (liii list) (srfi srfi-217))


(check-set-mode! 'report-failed)


(define pos-seq (iota 20 100 3))

(define neg-seq (iota 20 -100 3))

(define pos-set (list->iset pos-seq))

(define neg-set (list->iset neg-seq))


(check (iset->list (iset-difference (iset 0 1 3 4) (iset 0 2) (iset 0 4)))
  =>
  '(1 3)
) ;check
(check (iset->list (iset-difference pos-set neg-set)) => pos-seq)


(check-report)
