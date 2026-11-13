;; <
;; 判断实数是否严格递增。
;;
;; 语法
;; ----
;; (< x1 x2 ...)
;;
;; 参数
;; ----
;; x1, x2 : real? 实数。
;;
;; 返回值
;; ----
;; boolean? 严格递增返回 #t，否则返回 #f。

(import (liii check) (liii bignum))

(check-set-mode! 'report-failed)

(check (< (- (expt 10 30))
         0
         9223372036854775807
         (+ 9223372036854775807 1)
         (expt 10 30)
       ) ;<
  =>
  #t
) ;check

(check (< (expt 10 400) +inf.0) => #t)
(check (< -inf.0 (- (expt 10 400))) => #t)
(check (< 1e20 (+ (expt 10 20) 1)) => #t)
(check (< (expt 10 20) +nan.0) => #f)
(check (< (expt 10 20)) => #t)

(check-report)
