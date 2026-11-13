;; real-part
;; 获取复数的实部（支持精确大复数）。
;;
;; 语法
;; ----
;; (real-part z)
;;
;; 参数
;; ----
;; z : complex? 复数或实数。
;;
;; 返回值
;; ----
;; real? 实部。

(import (liii check) (liii bignum))

(check-set-mode! 'report-failed)

(check (real-part (make-rectangular (expt 10 20) 2)) => (expt 10 20))
(check (real-part (expt 10 20)) => (expt 10 20))

(check-report)
