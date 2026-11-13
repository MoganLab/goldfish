;; bigint-and
;; 返回两个 bigint 的按位与结果。
;;
;; 语法
;; ----
;; (bigint-and a b)
;;
;; 参数
;; ----
;; a : bigint?
;; 第一个操作数。
;; b : bigint?
;; 第二个操作数。
;;
;; 返回值
;; ----
;; bigint?
;; 按位与后的结果。
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
(check (bigint->string (bigint-and (string->bigint "6")
                                   (string->bigint "3")))
       => "2"
) ;check
(check (bigint->string (bigint-and (string->bigint "15")
                                   (string->bigint "0")))
       => "0"
) ;check
(check (bigint->string (bigint-and (string->bigint "-1")
                                   (string->bigint "3")))
       => "3"
) ;check
(check-catch 'type-error (bigint-and 1 (string->bigint "1")))

(check-report)
