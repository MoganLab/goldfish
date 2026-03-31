(import (liii check)
        (liii range)
) ;import

(check-set-mode! 'report-failed)

;; range-last
;; 获取 range 的最后一个元素。
;;
;; 语法
;; ----
;; (range-last r)
;;
;; 参数
;; ----
;; r : range
;; range 对象。
;;
;; 返回值
;; ----
;; any
;; 最后一个元素。
;;
;; 注意
;; ----
;; 等价于 (range-ref r (- (range-length r) 1))。
;;
;; 示例
;; ----
;; (range-last (numeric-range 10 20)) => 19
;;
;; 错误处理
;; ----
;; 无

(let ((r (numeric-range 10 20)))
  (check (range-last r) => 19)
) ;let

(let ((r (numeric-range 0 10)))
  (check (range-last r) => 9)
) ;let

(let ((r (numeric-range 5 0 -1)))
  (check (range-last r) => 1)
) ;let

(let ((r (vector-range #(a b c d e))))
  (check (range-last r) => 'e)
) ;let

(check-report)
