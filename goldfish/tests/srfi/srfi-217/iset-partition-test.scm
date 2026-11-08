;;
;; iset-partition
;; 将集合划分为满足和不满足谓词的两个集合。
;;

;; 语法
;; ----
;; (iset-partition predicate iset)
;;

;; 参数
;; ----
;; pred : procedure
;; 接受一个元素并返回布尔值的判定过程。
;;
;; set : iset
;; 待划分的集合。

;; 返回值
;; ----
;; 返回两个值：满足谓词的集合和不满足谓词的集合。
;;

(import (liii check) (srfi srfi-217))


(check-set-mode! 'report-failed)


(let-values (((low high) (iset-partition (lambda (x) (< x 6)) (iset 2 3 5 7 11))))
  (check (iset->list low) => '(2 3 5))
  (check (iset->list high) => '(7 11))
) ;let-values


(check-report)
