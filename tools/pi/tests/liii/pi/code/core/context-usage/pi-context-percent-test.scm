;; pi-context-percent
;; 计算上下文 Token 数占上下文窗口的百分比
;;
;; 语法
;; ----
;; (pi-context-percent tokens window)
;;
;; 参数
;; ----
;; tokens : number
;; 上下文 Token 数。
;; window : number | #f
;; 上下文窗口 Token 数。
;;
;; 返回值
;; ----
;; number | #f
;; 百分比（0-100+）；窗口缺失或非正数时返回 #f。

(import (scheme base) (liii check) (liii pi code core context-usage))

(check-set-mode! 'report-failed)

;; 1. 常规占比
(check (pi-context-percent 250000 1000000) => 25.0)
(check (pi-context-percent 524288 1048576) => 50.0)

;; 2. 窗口缺失或非正数时无法计算
(check (pi-context-percent 100 0) => #f)
(check (pi-context-percent 100 #f) => #f)

(check-report)
