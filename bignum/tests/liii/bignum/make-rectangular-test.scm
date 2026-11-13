;; make-rectangular
;; 根据实部与虚部构造复数（支持大整数与大有理数）。
;;
;; 语法
;; ----
;; (make-rectangular x y)
;;
;; 参数
;; ----
;; x : real? 实部。
;; y : real? 虚部。
;;
;; 返回值
;; ----
;; complex? 构造的复数（虚部为精确 0 时返回实部）。

(import (liii check) (liii bignum))

(check-set-mode! 'report-failed)

(check (make-rectangular (expt 10 20) 0) => (expt 10 20))
(check (number->string (make-rectangular (expt 10 20) 2))
  =>
  "100000000000000000000+2i"
) ;check

(check-report)
