;; format-range-unified
;; 把行号区间格式化为统一差异（Unified Diff）中的范围字符串。
;;
;; 语法
;; ----
;; (format-range-unified start stop)
;;
;; 参数
;; ----
;; start : exact-integer?
;; 区间起始行号，从 0 开始计数。
;; stop : exact-integer?
;; 区间结束行号，从 0 开始计数，不含该行。
;;
;; 返回值
;; ----
;; string
;; 单行区间返回起始行号字符串；
;; 多行区间返回 "起始行号,行数"；
;; 空区间返回 "起始行号,0"。
;;
;; 说明
;; ----
;; 行号按统一差异规范从 1 开始显示，
;; 而 start/stop 使用 0 基的半开区间下标。

(import (liii check)
        (liii diff hunk))

(check-set-mode! 'report-failed)

;; 单行区间只显示起始行号
(check (format-range-unified 0 1) => "1")
(check (format-range-unified 4 5) => "5")

;; 多行区间显示 "起始行号,行数"
(check (format-range-unified 2 5) => "3,3")
(check (format-range-unified 0 10) => "1,10")

;; 空区间显示 "起始行号,0"
(check (format-range-unified 0 0) => "0,0")
(check (format-range-unified 7 7) => "7,0")

(check-report)
