;; bigint-bit-flip
;; 翻转第 n 位。
;;
;; 语法
;; ----
;; (bigint-bit-flip a n)
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
;; 翻转后的结果。
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
(check (bigint->string (bigint-bit-flip (string->bigint "5") 1)) => "7")
(check (bigint->string (bigint-bit-flip (string->bigint "5") 0)) => "4")
(check (bigint->string (bigint-bit-flip (string->bigint "-8") 0)) => "-7")
(check (bigint->string (bigint-bit-flip (string->bigint "-1") 5)) => "-33")
(check-catch 'value-error (bigint-bit-flip (string->bigint "1") -1))

(check-report)
