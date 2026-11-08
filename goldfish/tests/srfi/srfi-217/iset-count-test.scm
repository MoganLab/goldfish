;;
;; iset-count
;; 计算集合中满足谓词的元素数量。
;;

;; 语法
;; ----
;; (iset-count predicate iset)
;;

;; 参数
;; ----
;; pred : procedure
;; 接受一个元素并返回布尔值的判定过程。
;;
;; set : iset
;; 待统计的集合。

;; 返回值
;; ----
;; integer
;; 返回满足 pred 的元素个数。

(import (liii check) (liii list) (srfi srfi-217))


(check-set-mode! 'report-failed)


(define pos-seq (iota 20 100 3))

(define pos-set (list->iset pos-seq))


(check (iset-count positive? (iset -2 -1 1 2)) => 2)
(check (iset-count even? (iset)) => 0)
(check (iset-count even? pos-set) => (count even? pos-seq))


(check-report)
