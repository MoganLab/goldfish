(import (liii check)
        (liii range)
) ;import

(check-set-mode! 'report-failed)

;; range-length
;; 获取 range 的长度。
;;
;; 语法
;; ----
;; (range-length r)
;;
;; 参数
;; ----
;; r : range
;; range 对象。
;;
;; 返回值
;; ----
;; exact-natural
;; range 的长度，非负整数。
;;
;; 示例
;; ----
;; (range-length (numeric-range 0 10)) => 10
;; (range-length (range 5 (lambda (i) i))) => 5
;; (range-length (numeric-range 0 0)) => 0
;;
;; 错误处理
;; ----
;; 无

(check (range-length (numeric-range 0 10)) => 10)
(check (range-length (numeric-range 0 5)) => 5)
(check (range-length (numeric-range 0 0)) => 0)
(check (range-length (range 5 (lambda (i) i))) => 5)
(check (range-length (range 0 (lambda (i) i))) => 0)

(let ((r (numeric-range 10 30 2)))
  (check (range-length r) => 10)
) ;let

(let ((r (numeric-range 5 0 -1)))
  (check (range-length r) => 5)
) ;let

(check-report)
