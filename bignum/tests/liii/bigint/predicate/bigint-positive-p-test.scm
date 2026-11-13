;; bigint-positive?
;; 判断 bigint 是否为正数。
;;
;; 语法
;; ----
;; (bigint-positive? n)
;;
;; 参数
;; ----
;; n : bigint?
;;   要判断的 bigint。
;;
;; 返回值
;; ----
;; boolean?
;;   正数返回 #t，否则返回 #f。
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

(check-true (bigint-positive? bigint-one))
(check-true (bigint-positive? (string->bigint "999999999999")))
(check-false (bigint-positive? bigint-zero))
(check-false (bigint-positive? (string->bigint "-1")))

(check-catch 'type-error (bigint-positive? 1))

(check-report)
