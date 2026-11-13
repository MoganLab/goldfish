;; round
;; 四舍六入五成双取整（支持大整数）。
;;
;; 语法
;; ----
;; (round x)
;;
;; 参数
;; ----
;; x : real? 实数。
;;
;; 返回值
;; ----
;; integer? 舍入结果。

(import (liii check) (liii bignum))

(check-set-mode! 'report-failed)

(check (round (expt 10 30)) => (expt 10 30))
(check
  (round
    (/ (+ (* 2 (expt 10 20)) 5) 2)
  ) ;round
  =>
  (+ (expt 10 20) 2)
) ;check

(check-report)
