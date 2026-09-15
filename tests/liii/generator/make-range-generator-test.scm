(import (liii check) (liii generator))

(check-set-mode! 'report-failed)

;; make-range-generator
;; 创建一个数值区间范围的生成器。
;;
;; 语法
;; ----
;; (make-range-generator start)
;; (make-range-generator start end)
;; (make-range-generator start end step)
;;
;; 参数
;; ----
;; start : number
;; 起始数值。
;;
;; end : number (可选)
;; 结束边界（不包含此值）。若未提供则为无限生成器。
;;
;; step : number (可选)
;; 步长值，默认为 1。
;;
;; 返回值
;; ----
;; procedure
;; 一个无参生成器过程。每次递增 step 并产出当前数值，当数值达到或超过 end 时返回 eof-object。
;;
;; 错误处理
;; ----
;; 无

(let ((g (make-range-generator 1 5 2)))
  (check (g) => 1)
  (check (g) => 3)
  (check-true (eof-object? (g)))
)

(let ((g (make-range-generator 0 3)))
  (check (g) => 0)
  (check (g) => 1)
  (check (g) => 2)
  (check-true (eof-object? (g)))
)

(let ((g (make-range-generator 10)))
  (check (g) => 10)
  (check (g) => 11)
  (check (g) => 12)
)

(check-report)
