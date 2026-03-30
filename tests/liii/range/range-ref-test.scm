(import (liii check)
        (srfi srfi-196)
) ;import

(check-set-mode! 'report-failed)

;; range-ref
;; 获取 range 中指定索引位置的元素。
;;
;; 语法
;; ----
;; (range-ref r index)
;;
;; 参数
;; ----
;; r : range
;; range 对象。
;;
;; index : exact-natural
;; 索引位置，从 0 开始。
;;
;; 返回值
;; ----
;; any
;; 索引位置的元素。
;;
;; 示例
;; ----
;; (range-ref (numeric-range 0 10) 0) => 0
;; (range-ref (numeric-range 0 10) 9) => 9
;;
;; 错误处理
;; ----
;; 无

(let ((r (numeric-range 0 10)))
  (check (range-ref r 0) => 0)
  (check (range-ref r 5) => 5)
  (check (range-ref r 9) => 9)
) ;let

(let ((r (numeric-range 10 30 2)))
  (check (range-ref r 0) => 10)
  (check (range-ref r 1) => 12)
  (check (range-ref r 9) => 28)
) ;let

(let ((r (range 10 (lambda (i) (* i 2)))))
  (check (range-ref r 0) => 0)
  (check (range-ref r 5) => 10)
  (check (range-ref r 9) => 18)
) ;let

(let ((r (vector-range #(a b c d e))))
  (check (range-ref r 0) => 'a)
  (check (range-ref r 4) => 'e)
) ;let

(check-report)
