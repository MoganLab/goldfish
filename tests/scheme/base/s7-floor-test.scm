(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)
;; s7-floor
;; S7 内置的向下取整函数，与 floor 行为相同。
;;
;; 语法
;; ----
;; (s7-floor x)
;;
;; 参数
;; ----
;; x : real?
;; 待取整的实数。
;;
;; 返回值
;; ------
;; real?
;; 不大于 x 的最大整数。
;;
;; 说明
;; ----
;; 1. 与 floor 行为一致
;; 2. 对于整数返回自身
(check (s7-floor 3.7) => 3)
(check (s7-floor -3.7) => -4)
(check (s7-floor 5) => 5)
(check (s7-floor 2.0) => 2)

(check-report)
