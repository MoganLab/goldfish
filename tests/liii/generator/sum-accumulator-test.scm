(import (liii check) (liii generator))

(check-set-mode! 'report-failed)

;; sum-accumulator
;; 创建对输入数值进行求和累加的累加器。
;;
;; 语法
;; ----
;; (sum-accumulator)
;;
;; 参数
;; ----
;; 无
;;
;; 返回值
;; ----
;; procedure
;; 一个单参累加器过程。传入数值时将其加到内部总和中；传入 eof-object 时返回累计和（初始为 0）。
;;
;; 错误处理
;; ----
;; 无

(let ((a (sum-accumulator)))
  (a 1)
  (a 2)
  (a 3)
  (check (a (eof-object)) => 6)
)

(let ((a (sum-accumulator)))
  (check (a (eof-object)) => 0)
)

(check-report)
