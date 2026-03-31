(import (liii check)
        (liii range)
) ;import

(check-set-mode! 'report-failed)

;; range-first
;; 获取 range 的第一个元素。
;;
;; 语法
;; ----
;; (range-first r)
;;
;; 参数
;; ----
;; r : range
;; range 对象。
;;
;; 返回值
;; ----
;; any
;; 第一个元素。
;;
;; 注意
;; ----
;; 等价于 (range-ref r 0)。
;;
;; 示例
;; ----
;; (range-first (numeric-range 10 20)) => 10
;;
;; 错误处理
;; ----
;; 无

(let ((r (numeric-range 10 20)))
  (check (range-first r) => 10)
) ;let

(let ((r (numeric-range 0 10)))
  (check (range-first r) => 0)
) ;let

(let ((r (vector-range #(a b c d e))))
  (check (range-first r) => 'a)
) ;let

(check-report)
