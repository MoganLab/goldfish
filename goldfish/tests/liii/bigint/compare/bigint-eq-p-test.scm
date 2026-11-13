;; bigint=?
;; 判断两个 bigint 是否相等。
;;
;; 语法
;; ----
;; (bigint=? a b)
;;
;; 参数
;; ----
;; a : bigint?
;;   第一个操作数。
;; b : bigint?
;;   第二个操作数。
;;
;; 返回值
;; ----
;; boolean?
;;   相等返回 #t，否则返回 #f。
;;
;; 错误处理
;; ----
;; type-error
;;   当任一参数不是 bigint 时抛出。

(import (liii check)
        (liii bigint compare)
        (liii bigint conversion)
) ;import

(check-set-mode! 'report-failed)

(check-true (bigint=? (string->bigint "123456789")
                      (string->bigint "123456789"))
) ;check-true
(check-false (bigint=? (string->bigint "123456789")
                       (string->bigint "-123456789"))
) ;check-false
(check-false (bigint=? (string->bigint "0") (string->bigint "1")))

(check-catch 'type-error (bigint=? 1 (string->bigint "1")))
(check-catch 'type-error (bigint=? (string->bigint "1") 1))

(check-report)
