;; bigint-bit-length
;; 返回表示该 bigint 所需的最小位数。
;;
;; 语法
;; ----
;; (bigint-bit-length a)
;;
;; 参数
;; ----
;; a : bigint?
;; 输入值。
;;
;; 返回值
;; ----
;; integer?
;; 不含符号位的最小位数。
;;
;; 错误处理
;; ----
;; type-error
;; 当参数不是 bigint 时抛出。

(import (liii check)
        (liii bigint bitwise)
        (liii bigint conversion)
) ;import

(check-set-mode! 'report-failed)
(check (bigint-bit-length (string->bigint "0")) => 0)
(check (bigint-bit-length (string->bigint "8")) => 4)
(check (bigint-bit-length (string->bigint "-1")) => 0)
(check (bigint-bit-length (string->bigint "-2")) => 1)
(check (bigint-bit-length (string->bigint "-3")) => 2)
(check-catch 'type-error (bigint-bit-length 8))

(check-report)
