;; nl
;; 换行组合子：无条件输出一个换行符。
;;
;; 语法
;; ----
;; nl
;;
;; 参数
;; ----
;; 无。
;;
;; 返回值
;; ----
;; format?
;; 输出单个换行符并把行号加一、列号归零的组合子。
;;
;; 说明
;; ----
;; 只在当前列不为 0 时才换行的组合子是 fl。
;; `(liii format base)` 以 Goldfish 命名重导出本函数。

(import (liii check) (srfi srfi-166 base))

(check-set-mode! 'report-failed)

(check (show #f "a" nl "b") => "a\nb")
(check (show #f nl) => "\n")
(check (show #f "a" nl (fn (row col) (each "[" row "," col "]"))) => "a\n[1,0]")

(check-report)
