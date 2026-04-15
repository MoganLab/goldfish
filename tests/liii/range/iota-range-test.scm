(import (liii check) (liii range))


(check-set-mode! 'report-failed)


;; iota-range
;; 创建 iota 序列的 range（类似于 iota 函数）。
;;
;; 语法
;; ----
;; (iota-range len)
;; (iota-range len start)
;; (iota-range len start step)
;;
;; 参数
;; ----
;; len : exact-natural
;; 序列长度。
;;
;; start : real (可选)
;; 起始值，默认为 0。
;;
;; step : real (可选)
;; 步长，默认为 1。
;;
;; 返回值
;; ----
;; range
;; 包含 iota 序列的 range 对象。
;;
;; 注意
;; ----
;; 创建等差数列，类似于 (start, start+step, start+2*step, ...)。
;;
;; 示例
;; ----
;; (iota-range 10) => 包含 0-9 的 range
;; (iota-range 5 10) => 包含 10,11,12,13,14 的 range
;; (iota-range 5 0 2) => 包含 0,2,4,6,8 的 range
;;
;; 错误处理
;; ----
;; 无


(let ((r (iota-range 10)))
  (check (range-length r) => 10)
  (check (range-ref r 0) => 0)
  (check (range-ref r 9) => 9)
) ;let


(let ((r (iota-range 5 10)))
  (check (range-length r) => 5)
  (check (range-ref r 0) => 10)
  (check (range-ref r 4) => 14)
) ;let


(let ((r (iota-range 5 0 2)))
  (check (range-length r) => 5)
  (check (range-ref r 0) => 0)
  (check (range-ref r 1) => 2)
  (check (range-ref r 4) => 8)
) ;let


(check-report)
