;; bigint-or
;; 返回两个 bigint 的按位或结果。
;;
;; 语法
;; ----
;; (bigint-or a b)
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
;; 按位或后的结果。
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
(check (bigint->string (bigint-or (string->bigint "6")
                                  (string->bigint "3")))
       => "7"
) ;check
(check (bigint->string (bigint-or (string->bigint "0")
                                  (string->bigint "9")))
       => "9"
) ;check
(check (bigint->string (bigint-or (string->bigint "-8")
                                  (string->bigint "7")))
       => "-1"
) ;check
(check-catch 'type-error (bigint-or (string->bigint "1") 2))

(check-report)
