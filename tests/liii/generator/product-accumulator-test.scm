(import (liii check) (liii generator))

(check-set-mode! 'report-failed)

;; product-accumulator
;; 创建对输入数值进行乘积累加的累加器。
;;
;; 语法
;; ----
;; (product-accumulator)
;;
;; 参数
;; ----
;; 无
;;
;; 返回值
;; ----
;; procedure
;; 一个单参累加器过程。传入数值时将其乘以内部乘积；传入 eof-object 时返回累计积（初始为 1）。
;;
;; 错误处理
;; ----
;; 无

(let ((a (product-accumulator)))
  (a 2)
  (a 3)
  (check (a (eof-object)) => 6)
)

(let ((a (product-accumulator)))
  (check (a (eof-object)) => 1)
)

(check-report)
