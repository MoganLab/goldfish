;; bigint-even?
;; 判断 bigint 是否为偶数。
;;
;; 语法
;; ----
;; (bigint-even? n)
;;
;; 参数
;; ----
;; n : bigint?
;;   输入值。
;;
;; 返回值
;; ----
;; boolean?
;;   为偶数时返回 #t。
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

(check-true (bigint-even? (string->bigint "12")))
(check-false (bigint-even? (string->bigint "5")))
(check-catch 'type-error (bigint-even? 12))

(check-report)
