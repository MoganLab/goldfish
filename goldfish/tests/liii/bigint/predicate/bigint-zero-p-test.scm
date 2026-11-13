;; bigint-zero?
;; 判断 bigint 是否为零。
;;
;; 语法
;; ----
;; (bigint-zero? n)
;;
;; 参数
;; ----
;; n : bigint?
;;   要判断的 bigint。
;;
;; 返回值
;; ----
;; boolean?
;;   零返回 #t，否则返回 #f。
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

(check-true (bigint-zero? bigint-zero))
(check-true (bigint-zero? (string->bigint "0")))
(check-false (bigint-zero? bigint-one))
(check-false (bigint-zero? (string->bigint "-1")))

(check-catch 'type-error (bigint-zero? 0))

(check-report)
