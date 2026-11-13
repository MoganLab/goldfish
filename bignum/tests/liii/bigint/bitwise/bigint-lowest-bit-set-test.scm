;; bigint-lowest-bit-set
;; 返回最低位 1 的索引。
;;
;; 语法
;; ----
;; (bigint-lowest-bit-set a)
;;
;; 参数
;; ----
;; a : bigint?
;; 输入值。
;;
;; 返回值
;; ----
;; integer?
;; 最低位 1 的索引；零返回 -1。
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
(check (bigint-lowest-bit-set (string->bigint "0")) => -1)
(check (bigint-lowest-bit-set (string->bigint "12")) => 2)
(check (bigint-lowest-bit-set (string->bigint "-8")) => 3)
(check (bigint-lowest-bit-set (string->bigint "-3")) => 0)
(check-catch 'type-error (bigint-lowest-bit-set 12))

(check-report)
