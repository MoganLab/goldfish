;; imag-part
;; 获取复数的虚部（支持精确大复数）。
;;
;; 语法
;; ----
;; (imag-part z)
;;
;; 参数
;; ----
;; z : complex? 复数或实数。
;;
;; 返回值
;; ----
;; real? 虚部。

(import (liii check) (liii bignum))

(check-set-mode! 'report-failed)

(check (imag-part (make-rectangular 1 (expt 10 20))) => (expt 10 20))
(check (imag-part (expt 10 20)) => 0)

(check-report)
