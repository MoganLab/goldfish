;; bigint-bit-set?
;; 判断第 n 位是否为 1。
;;
;; 语法
;; ----
;; (bigint-bit-set? a n)
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
;; boolean?
;; 指示该位是否置位。
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
(check-true (bigint-bit-set? (string->bigint "5") 0))
(check-false (bigint-bit-set? (string->bigint "5") 1))
(check-false (bigint-bit-set? (string->bigint "-2") 0))
(check-true (bigint-bit-set? (string->bigint "-2") 1))
(check-catch 'value-error (bigint-bit-set? (string->bigint "5") -1))

(check-report)
