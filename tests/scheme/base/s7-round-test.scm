(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)
;; s7-round
;; S7 内置的四舍五入函数，与 round 行为相同。
;;
;; 语法
;; ----
;; (s7-round x)
;;
;; 参数
;; ----
;; x : real?
;; 待四舍五入的实数。
;;
;; 返回值
;; ------
;; real?
;; 最接近 x 的整数，半数时向偶数舍入。
;;
;; 说明
;; ----
;; 1. 与 round 行为一致
;; 2. 半数时采用银行家舍入（向偶数靠近）
(check (s7-round 3.5) => 4)
(check (s7-round 2.5) => 2)
(check (s7-round -3.5) => -4)
(check (s7-round 3.2) => 3)
(check (s7-round 5) => 5)

(check-report)
