;; bigint-bit-count
;; 返回置位比特的数量。
;;
;; 语法
;; ----
;; (bigint-bit-count a)
;;
;; 参数
;; ----
;; a : bigint?
;; 输入值。
;;
;; 返回值
;; ----
;; integer?
;; 置位比特总数。
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
(check (bigint-bit-count (string->bigint "0")) => 0)
(check (bigint-bit-count (string->bigint "7")) => 3)
(check (bigint-bit-count (string->bigint "-1")) => 0)
(check (bigint-bit-count (string->bigint "-2")) => 1)
(check (bigint-bit-count (string->bigint "-4")) => 2)
(check-catch 'type-error (bigint-bit-count 7))

(check-report)
