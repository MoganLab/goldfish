;; >
;; 判断实数是否严格递减。
;;
;; 语法
;; ----
;; (> x1 x2 ...)
;;
;; 参数
;; ----
;; x1, x2 : real? 实数。
;;
;; 返回值
;; ----
;; boolean? 严格递减返回 #t，否则返回 #f。

(import (liii check) (liii bignum))

(check-set-mode! 'report-failed)

(check (> (expt 10 30)
         (+ 9223372036854775807 1)
         9223372036854775807
         0
         (- (expt 10 30))
       ) ;>
  =>
  #t
) ;check

(check (> (+ (expt 10 20) 1) 1e20) => #t)
(check (> (expt 10 400) +inf.0) => #f)
(check (> +inf.0 (expt 10 400)) => #t)

(check-report)
