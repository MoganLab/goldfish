(import (liii check)
        (srfi srfi-196)
) ;import

(check-set-mode! 'report-failed)

;; range->vector
;; 将 range 转换为向量。
;;
;; 语法
;; ----
;; (range->vector r)
;;
;; 参数
;; ----
;; r : range
;; range 对象。
;;
;; 返回值
;; ----
;; vector
;; 包含 range 所有元素的向量。
;;
;; 示例
;; ----
;; (range->vector (numeric-range 0 5)) => #(0 1 2 3 4)
;;
;; 错误处理
;; ----
;; 无

(let ((r (numeric-range 0 5)))
  (check (range->vector r) => #(0 1 2 3 4))
) ;let

(let ((r (numeric-range 0 0)))
  (check (range->vector r) => #())
) ;let

(let ((r (numeric-range 10 20 2)))
  (check (range->vector r) => #(10 12 14 16 18))
) ;let

(let ((r (vector-range #(a b c d e))))
  (check (range->vector r) => #(a b c d e))
) ;let

(check-report)
