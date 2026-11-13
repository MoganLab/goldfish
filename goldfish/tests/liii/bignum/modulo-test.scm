;; modulo
;; 计算整数除法的模（支持大整数）。
;;
;; 语法
;; ----
;; (modulo n1 n2)
;;
;; 参数
;; ----
;; n1, n2 : integer? 整数。
;;
;; 返回值
;; ----
;; integer? 模。

(import (liii check) (liii bignum))

(check-set-mode! 'report-failed)

(check
  (modulo (- (+ (expt 10 30) 7)) 10)
  =>
  3
) ;check

(check
  (modulo 100000000000000000005 -10)
  =>
  -5
) ;check

(check
  (modulo -100000000000000000005 -10)
  =>
  -5
) ;check

(check
  (modulo 100000000000000000000 -10)
  =>
  0
) ;check

(check (modulo (+ (expt 10 20) 5) 3.0) => 0.0)

(check-report)
