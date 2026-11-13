;; bigint-bit-clear
;; 将第 n 位清零。
;;
;; 语法
;; ----
;; (bigint-bit-clear a n)
;;
;; 参数
;; ----
;; a : bigint?
;; 输入值。
;; n : integer?
;; 从 0 开始的位索引。
;;
;; 返回值
;; ----
;; bigint?
;; 清零后的结果。
;;
;; 错误处理
;; ----
;; type-error / value-error
;; 当参数不是 bigint 时抛出 type-error；位索引为负时抛出 value-error。

(import (liii check)
        (liii bigint bitwise)
        (liii bigint conversion)
) ;import

(check-set-mode! 'report-failed)
(check (bigint->string (bigint-bit-clear (string->bigint "7") 1)) => "5")
(check (bigint->string (bigint-bit-clear (string->bigint "5") 1)) => "5")
(check (bigint->string (bigint-bit-clear (string->bigint "-8") 3)) => "-16")
(check (bigint->string (bigint-bit-clear (string->bigint "-1") 5)) => "-33")
(check-catch 'value-error (bigint-bit-clear (string->bigint "1") -1))

(check-report)
