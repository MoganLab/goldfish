;; numeric/fitted
;; 定宽数值组合子：先按 numeric 格式化再定宽输出。
;;
;; 语法
;; ----
;; (numeric/fitted width n . rest)
;;
;; 参数
;; ----
;; width : exact-integer?
;; 目标显示宽度。
;;
;; n : number?
;; 待格式化的数值。
;;
;; rest : any
;; 透传给 numeric 的可选参数（radix、precision 等）。
;;
;; 返回值
;; ----
;; format?
;; 输出占 width 列数字字符串的组合子。
;;
;; 说明
;; ----
;; 当格式化结果放不下时，超出的高位用 `#` 标记。
;; `(liii format base)` 以 Goldfish 命名重导出本函数。

(import (liii check) (srfi srfi-166 base))

(check-set-mode! 'report-failed)

(check (show #f (numeric/fitted 8 1.5 10 2)) => "1.50")
(check (show #f (numeric/fitted 3 1.2345 10 2)) => ".##")

(check-report)
