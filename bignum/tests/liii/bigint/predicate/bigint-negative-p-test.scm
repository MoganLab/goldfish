;; bigint-negative?
;; 判断 bigint 是否为负数。
;;
;; 语法
;; ----
;; (bigint-negative? n)
;;
;; 参数
;; ----
;; n : bigint?
;;   要判断的 bigint。
;;
;; 返回值
;; ----
;; boolean?
;;   负数返回 #t，否则返回 #f。
;;
;; 错误处理
;; ----
;; type-error
;;   当参数不是 bigint 时抛出。

(import (liii check)
        (liii bigint predicate)
        (liii bigint constant)
        (liii bigint conversion)
) ;import

(check-set-mode! 'report-failed)

(check-true (bigint-negative? (string->bigint "-1")))
(check-true (bigint-negative? (string->bigint "-999999999999")))
(check-false (bigint-negative? bigint-zero))
(check-false (bigint-negative? bigint-one))

(check-catch 'type-error (bigint-negative? -1))

(check-report)
