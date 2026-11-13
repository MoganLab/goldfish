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

(check-report)
