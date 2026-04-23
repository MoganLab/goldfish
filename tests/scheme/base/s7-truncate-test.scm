(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)
;; s7-truncate
;; S7 内置的向零截断函数，与 truncate 行为相同。
;;
;; 语法
;; ----
;; (s7-truncate x)
;;
;; 参数
;; ----
;; x : real?
;; 待截断的实数。
;;
;; 返回值
;; ------
;; real?
;; 向零方向截断后的整数。
;;
;; 说明
;; ----
;; 1. 与 truncate 行为一致
;; 2. 对于整数返回自身
(check (s7-truncate 3.7) => 3)
(check (s7-truncate -3.7) => -3)
(check (s7-truncate 5) => 5)
(check (s7-truncate 2.0) => 2)

(check-report)
