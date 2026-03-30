(import (liii check)
        (srfi srfi-196)
) ;import

(check-set-mode! 'report-failed)

;; range=?
;; 比较多个 range 是否相等。
;;
;; 语法
;; ----
;; (range=? equal r1 r2)
;; (range=? equal r1 r2 ...)
;;
;; 参数
;; ----
;; equal : procedure
;; 比较两个元素是否相等的函数。
;;
;; r1, r2, ... : range
;; 要比较的 range 对象。
;;
;; 返回值
;; ----
;; boolean
;; 如果所有 range 长度相同且对应元素相等，返回 #t，否则返回 #f。
;;
;; 示例
;; ----
;; (range=? = (numeric-range 0 5) (numeric-range 0 5)) => #t
;; (range=? = (numeric-range 0 5) (numeric-range 1 6)) => #f
;;
;; 错误处理
;; ----
;; 无

(let ((r1 (numeric-range 0 5))
      (r2 (numeric-range 0 5))
      (r3 (numeric-range 1 6)))
  (check-true (range=? = r1 r2))
  (check-false (range=? = r1 r3))
) ;let

(let ((r1 (numeric-range 10 20))
      (r2 (numeric-range 10 21)))
  (check-false (range=? = r1 r2))
) ;let

(check-report)
