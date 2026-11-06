;; numeric/comma
;; 千分位数值组合子：只负责插入分组分隔符。
;;
;; 语法
;; ----
;; (numeric/comma n [comma-rule])
;;
;; 参数
;; ----
;; n : number?
;; 待格式化的数值。
;;
;; comma-rule : exact-integer?
;; 可选，每几位分组一次，默认取状态变量 comma-rule。
;;
;; 返回值
;; ----
;; format?
;; 输出带分组分隔符数字的组合子。
;;
;; 说明
;; ----
;; 分隔符字符由状态变量 comma-sep 决定（默认逗号）。
;; `(liii format base)` 以 Goldfish 命名重导出本函数。

(import (liii check) (srfi srfi-166 base))

(check-set-mode! 'report-failed)

(check (show #f (numeric/comma 1234567)) => "1,234,567")
(check (show #f (numeric/comma 123)) => "123")
(check (show #f (numeric/comma 1234567 4)) => "123,4567")
(check
  (show #f (with ((comma-sep #\.)) (numeric/comma 1234567)))
  =>
  "1.234.567"
) ;check

(check-report)
