;;
;; iset-remove
;; 返回仅包含不满足谓词元素的新集合。
;;

;; 语法
;; ----
;; (iset-remove predicate iset)
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
;; 返回由不满足 pred 的元素组成的新 iset。

(import (liii check) (srfi srfi-217))


(check-set-mode! 'report-failed)


(check (iset->list (iset-remove (lambda (x) (< x 6)) (iset 2 3 5 7 11)))
  =>
  '(7 11)
) ;check


(check-report)
