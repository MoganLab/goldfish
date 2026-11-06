;;
;; iset->list
;; 将集合转换为有序列表。
;;

;; 语法
;; ----
;; (iset->list iset)
;;

;; 参数
;; ----
;; set : iset
;; 待转换的集合。

;; 返回值
;; ----
;; 返回按递增顺序排列的元素列表。
;;

(import (liii check) (liii list) (srfi srfi-217))


(check-set-mode! 'report-failed)


(define pos-seq (iota 20 100 3))

(define neg-seq (iota 20 -100 3))

(define pos-set (list->iset pos-seq))

(define neg-set (list->iset neg-seq))


(check (iset->list (iset)) => '())
(check (iset->list pos-set) => pos-seq)
(check (iset->list neg-set) => neg-seq)


(check-report)
