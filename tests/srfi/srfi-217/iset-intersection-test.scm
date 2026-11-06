;;
;; iset-intersection
;; 返回多个集合的交集。
;;

;; 语法
;; ----
;; (iset-intersection iset1 iset2 ...)
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
;; 返回两个集合交集组成的新 iset。

(import (liii check) (liii list) (srfi srfi-217))


(check-set-mode! 'report-failed)


(define pos-seq (iota 20 100 3))

(define neg-seq (iota 20 -100 3))

(define pos-set (list->iset pos-seq))

(define neg-set (list->iset neg-seq))


(check (iset->list (iset-intersection (iset 0 1 3 4) (iset 0 2 4))) => '(0 4))
(check-true (iset-empty? (iset-intersection pos-set neg-set)))


(check-report)
