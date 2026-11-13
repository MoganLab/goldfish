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

(check (imag-part 1+100000000000000000000i) => 100000000000000000000)
(check (imag-part 100000000000000000000) => 0)

(check-report)
