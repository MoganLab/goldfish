;; truncate/
;; 计算向零截断除法的商与余数（支持大整数与大有理数）。
;;
;; 语法
;; ----
;; (truncate/ n1 n2)
;;
;; 参数
;; ----
;; n1, n2 : real? 实数。
;;
;; 返回值
;; ----
;; values 两个值 q 和 r。

(import (liii check) (liii bignum))

(check-set-mode! 'report-failed)

(check
  (call-with-values
    (lambda () (truncate/ (- (expt 10 20)) 3))
    list
  ) ;call-with-values
  =>
  (list (- (quotient (expt 10 20) 3)) -1)
) ;check

(check-report)
