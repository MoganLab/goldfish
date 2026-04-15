(import (liii check) (liii iset))


(check-set-mode! 'report-failed)


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
;; 注意
;; ----
;; 按递增数值顺序应用 proc。
;;
(check (let ((sum 0))
         (iset-for-each (lambda (x) (set! sum (+ sum x)))
           (iset 2 3 5 7 11)
         ) ;iset-for-each
         sum
       ) ;let
  =>
  28
) ;check


(check-report)
