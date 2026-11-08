;;
;; iset-size
;; 返回集合中元素的数量。
;;

;; 语法
;; ----
;; (iset-size iset)
;;

;; 参数
;; ----
;; set : iset
;; 待统计的集合。

;; 返回值
;; ----
;; integer
;; 返回集合中的元素个数。

(import (liii check) (liii list) (srfi srfi-217))


(check-set-mode! 'report-failed)


(define pos-seq (iota 20 100 3))

(define pos-set (list->iset pos-seq))


(check (iset-size (iset)) => 0)
(check (iset-size (iset 1 3 5)) => 3)
(check (iset-size pos-set) => (length pos-seq))


(check-report)
