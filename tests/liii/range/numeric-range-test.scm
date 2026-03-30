(import (liii check)
        (srfi srfi-196)
) ;import

(check-set-mode! 'report-failed)

;; numeric-range
;; 创建数值范围的 range。
;;
;; 语法
;; ----
;; (numeric-range start end)
;; (numeric-range start end step)
;;
;; 参数
;; ----
;; start : real
;; 范围的起始值。
;;
;; end : real
;; 范围的结束值（不包含）。
;;
;; step : real (可选)
;; 步长，默认为 1。
;;
;; 返回值
;; ----
;; range
;; 包含数值序列的 range 对象。
;;
;; 注意
;; ----
;; 创建等差数列的 range。当 step 为负数时，可以创建递减序列。
;;
;; 示例
;; ----
;; (numeric-range 0 10) => 包含 0-9 的 range
;; (numeric-range 10 30 2) => 包含 10,12,14,...,28 的 range
;; (numeric-range 5 0 -1) => 包含 5,4,3,2,1 的 range
;;
;; 错误处理
;; ----
;; 无

(let ((r (numeric-range 0 10)))
  (check-true (range? r))
  (check (range-length r) => 10)
  (check (range-ref r 0) => 0)
  (check (range-ref r 9) => 9)
) ;let

(let ((r (numeric-range 10 30 2)))
  (check (range-length r) => 10)
  (check (range-ref r 0) => 10)
  (check (range-ref r 1) => 12)
  (check (range-ref r 9) => 28)
) ;let

(let ((r (numeric-range 5 0 -1)))
  (check (range-length r) => 5)
  (check (range-ref r 0) => 5)
  (check (range-ref r 4) => 1)
) ;let

(check-report)
