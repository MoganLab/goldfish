(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)
;; s7-ceiling
;; S7 内置的向上取整函数，与 ceiling 行为相同。
;;
;; 语法
;; ----
;; (s7-ceiling x)
;;
;; 参数
;; ----
;; x : real?
;; 待取整的实数。
;;
;; 返回值
;; ------
;; real?
;; 不小于 x 的最小整数。
;;
;; 说明
;; ----
;; 1. 与 ceiling 行为一致
;; 2. 对于整数返回自身
(check (s7-ceiling 3.2) => 4)
(check (s7-ceiling -3.2) => -3)
(check (s7-ceiling 5) => 5)
(check (s7-ceiling 2.0) => 2)

(check-report)
