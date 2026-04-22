(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)
;; s7-lcm
;; S7 内置的最小公倍数函数，与 lcm 行为相同。
;;
;; 语法
;; ----
;; (s7-lcm n1 ...)
;;
;; 参数
;; ----
;; n1 ... : integer?
;; 一个或多个整数。
;;
;; 返回值
;; ------
;; integer?
;; 参数的最小公倍数。
;;
;; 说明
;; ----
;; 1. 与 lcm 行为一致
;; 2. 无参数时返回 1
;; 3. 单参数时返回该参数的绝对值
(check (s7-lcm 4 6) => 12)
(check (s7-lcm 3 5 7) => 105)
(check (s7-lcm) => 1)
(check (s7-lcm 7) => 7)
(check (s7-lcm -4 6) => 12)

(check-report)
