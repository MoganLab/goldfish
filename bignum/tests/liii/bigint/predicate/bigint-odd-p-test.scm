;; bigint-odd?
;; 判断 bigint 是否为奇数。
;;
;; 语法
;; ----
;; (bigint-odd? n)
;;
;; 参数
;; ----
;; n : bigint?
;;   输入值。
;;
;; 返回值
;; ----
;; boolean?
;;   为奇数时返回 #t。
;;
;; 错误处理
;; ----
;; type-error
;;   当参数不是 bigint 时抛出。

(import (liii check)
        (liii bigint predicate)
        (liii bigint conversion)
) ;import

(check-set-mode! 'report-failed)

(check-true (bigint-odd? (string->bigint "5")))
(check-false (bigint-odd? (string->bigint "12")))
(check-catch 'type-error (bigint-odd? 5))

(check-report)
