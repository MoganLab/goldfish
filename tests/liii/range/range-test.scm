(import (liii check)
        (srfi srfi-196)
) ;import

(check-set-mode! 'report-failed)

;; range
;; 使用指定的长度和索引器函数创建 range。
;;
;; 语法
;; ----
;; (range length indexer)
;;
;; 参数
;; ----
;; length : exact-natural
;; range 的长度，必须是非负整数。
;;
;; indexer : procedure
;; 接受一个索引参数并返回对应位置的值的函数。
;;
;; 返回值
;; ----
;; range
;; 新创建的 range 对象。
;;
;; 注意
;; ----
;; 创建一个惰性序列，元素通过索引器函数按需计算。
;;
;; 示例
;; ----
;; (range 10 (lambda (i) (* i 2))) => 一个包含 10 个元素的 range
;;
;; 错误处理
;; ----
;; 无

(let ((r (range 10 (lambda (i) (* i 2)))))
  (check-true (range? r))
  (check (range-length r) => 10)
  (check (range-ref r 0) => 0)
  (check (range-ref r 5) => 10)
  (check (range-ref r 9) => 18)
) ;let

(check-report)
