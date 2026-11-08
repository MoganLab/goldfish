;;
;; iset-filter
;; 返回仅包含满足谓词元素的新集合。
;;

;; 语法
;; ----
;; (iset-filter predicate iset)
;;

;; 参数
;; ----
;; pred : procedure
;; 接受一个元素并返回布尔值的判定过程。
;;
;; set : iset
;; 待过滤的集合。

;; 返回值
;; ----
;; iset
;; 返回由满足 pred 的元素组成的新 iset。

(import (liii check) (srfi srfi-217))


(check-set-mode! 'report-failed)


(check (iset->list (iset-filter (lambda (x) (< x 6)) (iset 2 3 5 7 11)))
  =>
  '(2 3 5)
) ;check


(check-report)
