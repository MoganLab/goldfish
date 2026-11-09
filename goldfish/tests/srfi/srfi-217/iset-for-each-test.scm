;;
;; iset-for-each
;; 对集合中的每个元素应用函数，忽略返回值。
;;

;; 语法
;; ----
;; (iset-for-each proc iset)
;;

;; 参数
;; ----
;; proc : procedure
;; 要应用的函数。
;;
;; iset : iset
;; 目标集合。
;;

;; 返回值
;; ----
;; unspecified
;; 无返回值；对集合中每个元素调用 proc 以产生副作用。

;; 注意
;; ----
;; 按递增数值顺序应用 proc。
;;

(import (liii check) (srfi srfi-217))


(check-set-mode! 'report-failed)


(check (let ((sum 0))
         (iset-for-each (lambda (x) (set! sum (+ sum x))) (iset 2 3 5 7 11))
         sum
       ) ;let
  =>
  28
) ;check


(check-report)
